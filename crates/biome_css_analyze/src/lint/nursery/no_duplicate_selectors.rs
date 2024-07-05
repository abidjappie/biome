use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
    vec,
};

use biome_analyze::{
    context::RuleContext, declare_rule, Phases, QueryMatch, Queryable, Rule, RuleDiagnostic,
    Visitor, VisitorContext,
};
use biome_console::markup;
use biome_css_syntax::{
    AnyCssRelativeSelector, AnyCssSelector, AnyCssSimpleSelector, AnyCssSubSelector, CssLanguage,
    CssNestedQualifiedRule, CssQualifiedRule, CssRuleList, CssSyntaxKind,
};

use biome_rowan::{AstNode, SyntaxNode, TextRange, WalkEvent};

use biome_css_syntax::CssSyntaxNode;

declare_rule! {
    /// Disallow duplicate selectors.
    ///
    /// The same selector is allowed to repeat in the following circumstances:
    /// - It is used in different selector lists, e.g. `a {} a, b {}`.
    /// - The duplicates are in rules with different parent nodes, e.g. inside and outside of a media query.
    ///
    /// This rule resolves nested selectors. So `a b {} a { & b {} }` counts as a problem, because the resolved selectors end up with a duplicate.
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```css,expect_diagnostic
    /// .abc,
    /// .def,
    /// .abc { /* declaration */ }
    /// ```
    ///
    /// ### Valid
    ///
    /// ```
    /// .foo { /* declaration */ }
    /// .bar { /* declaration */ }
    /// ```
    ///
    pub NoDuplicateSelectors {
        version: "next",
        name: "noDuplicateSelectors",
        language: "css",
        recommended: false,
    }
}

#[derive(Eq, Debug, Clone)]
pub struct ResolvedSelectorWithTextRanges {
    resolved_string: String,
    first_seen_text_range: TextRange,
    text_ranges: Vec<TextRange>,
}

impl PartialEq for ResolvedSelectorWithTextRanges {
    fn eq(&self, other: &ResolvedSelectorWithTextRanges) -> bool {
        self.resolved_string == other.resolved_string
    }
}
impl Hash for ResolvedSelectorWithTextRanges {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.resolved_string.hash(state);
    }
}

#[derive(Default)]
struct DuplicateSelectorsVisitor {
    resolved: HashSet<ResolvedSelectorWithTextRanges>,
    parents: Vec<Vec<String>>,
}

impl Visitor for DuplicateSelectorsVisitor {
    type Language = CssLanguage;

    fn visit(
        &mut self,
        event: &WalkEvent<SyntaxNode<Self::Language>>,
        mut ctx: VisitorContext<Self::Language>,
    ) {
        match event {
            WalkEvent::Enter(node) => {
                if let Some(rule) = CssQualifiedRule::cast_ref(node) {
                    let normalized_selectors: Vec<Vec<String>> = rule
                        .prelude()
                        .into_iter()
                        .filter_map(|selector| selector.ok())
                        .map(|selector| {
                            if let AnyCssSelector::CssComplexSelector(sel) = selector.clone() {
                                // Only handle the " " case which should result in too selectors being output
                                if sel.combinator().is_ok_and(|combinator| {
                                    combinator.kind() == CssSyntaxKind::CSS_SPACE_LITERAL
                                }) {
                                    if let Ok(left) = sel.left() {
                                        if let Ok(right) = sel.right() {
                                            return vec![
                                                normalize_selectors_to_string(left.into_syntax()),
                                                normalize_selectors_to_string(right.into_syntax()),
                                            ];
                                        }
                                    }
                                }
                            }
                            vec![normalize_selectors_to_string(selector.into_syntax())]
                        })
                        .collect();
                    self.parents.extend(normalized_selectors);
                }
                if let Some(rule) = CssNestedQualifiedRule::cast_ref(node) {
                    let normalized_selectors: Vec<String> = rule
                        .prelude()
                        .into_iter()
                        .filter_map(|selector| selector.ok())
                        .map(|selector| normalize_selectors_to_string(selector.into_syntax()))
                        .collect();
                    self.parents.push(normalized_selectors);
                }
                if let Some(selector) = AnyCssSelector::cast_ref(node) {
                    // Don't handle ComplexSelectors
                    if let AnyCssSelector::CssComplexSelector(_sel) = selector.clone() {
                    } else {
                        let mut parents = self.parents.clone();
                        parents.pop();
                        if let Some(resolved) = parents.into_iter().reduce(|acc, cur| {
                            let mut temp = vec![];
                            for c in cur {
                                for a in acc.clone() {
                                    temp.push(a + " " + &c);
                                }
                            }
                            temp
                        }) {
                            for r in resolved {
                                let resolved_string = r + " " + &selector.text();
                                if let Some(occurence) =
                                    self.resolved.get(&ResolvedSelectorWithTextRanges {
                                        resolved_string: resolved_string.clone(),
                                        first_seen_text_range: selector.range(),
                                        text_ranges: [].to_vec(),
                                    })
                                {
                                    self.resolved.replace(ResolvedSelectorWithTextRanges {
                                        resolved_string: resolved_string,
                                        first_seen_text_range: occurence.first_seen_text_range,
                                        text_ranges: [
                                            occurence.text_ranges.clone(),
                                            [selector.range()].to_vec(),
                                        ]
                                        .concat(),
                                    });
                                } else {
                                    self.resolved.insert(ResolvedSelectorWithTextRanges {
                                        resolved_string: resolved_string.clone(),
                                        first_seen_text_range: selector.range(),
                                        text_ranges: [].to_vec(),
                                    });
                                }
                            }
                        };
                    }
                }
            }
            WalkEvent::Leave(node) => {
                if CssQualifiedRule::can_cast(node.kind())
                    || CssNestedQualifiedRule::can_cast(node.kind())
                {
                    self.parents.pop();
                }
                if CssRuleList::can_cast(node.kind()) {
                    for i in self.resolved.drain() {
                        if i.text_ranges.len() > 0 {
                            ctx.match_query(DuplicateSelectors(i));
                        }
                    }
                }
            }
        }
    }
}

pub struct DuplicateSelectors(ResolvedSelectorWithTextRanges);

impl QueryMatch for DuplicateSelectors {
    fn text_range(&self) -> TextRange {
        self.0.text_ranges[0]
    }
}

impl Queryable for DuplicateSelectors {
    type Input = Self;
    type Output = ResolvedSelectorWithTextRanges;
    type Language = CssLanguage;
    type Services = ();

    fn build_visitor(
        analyzer: &mut impl biome_analyze::AddVisitor<Self::Language>,
        _root: &<Self::Language as biome_rowan::Language>::Root,
    ) {
        analyzer.add_visitor(Phases::Syntax, DuplicateSelectorsVisitor::default);
    }

    fn unwrap_match(_services: &biome_analyze::ServiceBag, query: &Self::Input) -> Self::Output {
        query.0.clone()
    }
}

impl Rule for NoDuplicateSelectors {
    type Query = DuplicateSelectors;
    type State = ResolvedSelectorWithTextRanges;
    type Signals = Option<Self::State>;
    type Options = ();

    fn run(ctx: &RuleContext<Self>) -> Option<Self::State> {
        let resolved_duplicate_selector_with_range = ctx.query();
        Some(resolved_duplicate_selector_with_range.clone())
    }

    fn diagnostic(_: &RuleContext<Self>, node: &Self::State) -> Option<RuleDiagnostic> {
        Some(
            RuleDiagnostic::new(
                rule_category!(),
                node.first_seen_text_range,
                markup! {
                    "Duplicate selectors may result in unintentionally overriding rules."
                },
            )
            .details(
                node.text_ranges.clone(),
                "Please consider moving the rule's contents to the first occurence:",
            )
            .note(markup! {
                "Remove duplicate selectors within the rule"
            }),
        )
    }
}

/// This function trims the trivia off all selectors and joins the text representation of those selectors into a single string.
fn normalize_selectors_to_string(selector: CssSyntaxNode) -> String {
    let mut stack = vec![selector];
    let mut normalized = vec![];

    while let Some(node) = stack.pop() {
        if let Some(node) = AnyCssSimpleSelector::cast_ref(&node) {
            if let Some(node) = node.trim_trivia() {
                normalized.push(node.text());
            }
        }
        if let Some(node) = AnyCssSubSelector::cast_ref(&node) {
            if let Some(node) = node.trim_trivia() {
                // TODO: more advanced handling
                normalized.push(node.text());
            }
        }
        if let Some(node) = AnyCssRelativeSelector::cast_ref(&node) {
            match node {
                AnyCssRelativeSelector::CssBogusSelector(_) => todo!(),
                AnyCssRelativeSelector::CssRelativeSelector(selector) => {
                    // TODO: handle combinator
                    // This is where we handle ">" but not "&" which is handled inside of compound
                    if let Ok(selector) = selector.selector() {
                        stack.push(selector.into_syntax());
                    }
                }
            }
        }
        if let Some(node) = AnyCssSelector::cast_ref(&node) {
            match node {
                AnyCssSelector::CssBogusSelector(_) => todo!(),
                AnyCssSelector::CssComplexSelector(_selector) => {
                    // Do not handle the " "
                    // if let Ok(left) = selector.left() {
                    //     stack.push(left.into_syntax());
                    // }
                    // // TODO: some how handle the combinator too
                    // // Different combinators will result in different behaviors, e.g.
                    // // A space result in two resolved paths
                    // if let Ok(right) = selector.right() {
                    //     stack.push(right.into_syntax());
                    // }
                }
                AnyCssSelector::CssCompoundSelector(selector) => {
                    // TODO: Nesting selector is where we handle "&"
                    if let Some(simple) = selector.simple_selector() {
                        stack.push(simple.into_syntax());
                    }
                    for sub_selector in selector.sub_selectors() {
                        stack.push(sub_selector.into_syntax())
                    }
                }
            }
        }
    }
    normalized.join(" ")
}
