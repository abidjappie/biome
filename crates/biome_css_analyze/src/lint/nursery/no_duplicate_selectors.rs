use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::vec;

use biome_analyze::Ast;
use biome_analyze::{context::RuleContext, declare_rule, Rule, RuleDiagnostic, RuleSource};
use biome_console::markup;
use biome_css_syntax::{
    AnyCssAtRule, AnyCssRelativeSelector, AnyCssRule, AnyCssSelector, CssComplexSelector,
    CssRelativeSelector, CssRelativeSelectorList, CssRoot, CssSelectorList,
    CssSyntaxNode,
};
use biome_deserialize_macros::Deserializable;
use biome_rowan::{AstNode, SyntaxNodeCast};

use serde::{Deserialize, Serialize};

declare_rule! {
    /// Disallow duplicate selectors.
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```css,expect_diagnostic
    /// .abc,
    /// .def,
    /// .abc {}
    /// ```
    ///
    /// ### Valid
    ///
    /// ```
    /// .foo {}
    /// .bar {}
    /// ```
    ///
    /// ## Options
    ///
    /// If true, disallow duplicate selectors within selector lists.
    ///
    /// ```json
    /// {
    ///     "noDuplicateSelectors": {
    ///         "options": {
    ///           "disallowInList": true
    ///         }
    ///     }
    /// }
    /// ```
    ///
    pub NoDuplicateSelectors {
        version: "next",
        name: "noDuplicateSelectors",
        recommended: true,
        sources: &[RuleSource::Stylelint("no-duplicate-selectors")],
    }
}

#[derive(Debug, Clone, Deserialize, Deserializable, Eq, PartialEq, Serialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct NoDuplicateSelectorsOptions {
    pub disallow_in_list: bool,
}

impl Default for NoDuplicateSelectorsOptions {
    fn default() -> Self {
        Self {
            disallow_in_list: false,
        }
    }
}

#[derive(Debug, Eq)]
struct ResolvedSelector {
    selector_text: String,
    selector_node: CssSyntaxNode,
}

impl PartialEq for ResolvedSelector {
    fn eq(&self, other: &ResolvedSelector) -> bool {
        self.selector_text == other.selector_text
    }
}
impl Hash for ResolvedSelector {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.selector_text.hash(state);
    }
}
impl Borrow<String> for ResolvedSelector {
    fn borrow(&self) -> &String {
        &self.selector_text
    }
}

pub struct DuplicateSelector {
    first: CssSyntaxNode,
    duplicate: CssSyntaxNode,
}

impl Rule for NoDuplicateSelectors {
    type Query = Ast<CssRoot>;
    type State = DuplicateSelector;
    type Signals = Vec<Self::State>;
    type Options = NoDuplicateSelectorsOptions;

    fn run(ctx: &RuleContext<Self>) -> Vec<Self::State> {
        let node = ctx.query();
        let options = ctx.options();

        let mut resolved_list: HashSet<ResolvedSelector> = HashSet::new();
        let mut output: Vec<DuplicateSelector> = vec![];

        if options.disallow_in_list {
            let selectors = node.rules().syntax().descendants().filter(|x| {
                x.clone().cast::<AnyCssSelector>().is_some()
                    || x.clone().cast::<AnyCssRelativeSelector>().is_some()
            });

            // selector_list unwrap should never fail due to the structure of the AST
            for (selector, selector_list) in selectors
                .map(|selector| (selector.clone(), selector.parent().unwrap()))
                .filter(|(_, parent)| {
                    // i.e not actually a list
                    return !(parent.clone().cast::<CssComplexSelector>().is_some()
                        || parent.clone().cast::<CssRelativeSelector>().is_some());
                })
            {
                // this_rule unwrap should never fail due to the structure of the AST
                let this_rule = selector_list.parent().unwrap();

                let selector_text = if let Some(selector) = CssRelativeSelector::cast_ref(&selector)
                {
                    selector.clone().text()
                } else {
                    // selector is either AnyCssSelector or AnyCssRelativeSelector
                    normalize_complex_selector(selector.clone().cast::<AnyCssSelector>().unwrap())
                };

                for r in resolve_nested_selectors(selector_text, this_rule) {
                    let split: Vec<&str> = r.split_whitespace().collect();
                    let normalized = split.join(" ").to_lowercase();

                    if let Some(first) = resolved_list.get(&normalized) {
                        output.push(DuplicateSelector {
                            first: first.selector_node.clone(),
                            duplicate: selector.clone(),
                        });
                    } else {
                        resolved_list.insert(ResolvedSelector {
                            selector_text: normalized.clone(),
                            selector_node: selector.clone(),
                        });
                    }
                }
            }
        } else {
            let selector_lists = node.rules().syntax().descendants().filter(|x| {
                x.clone().cast::<CssSelectorList>().is_some()
                    || x.clone().cast::<CssRelativeSelectorList>().is_some()
            });

            // this_rule unwrap should never fail due to the structure of the AST
            for (selector_list, rule) in selector_lists
                .map(|selector_list| (selector_list.clone(), selector_list.parent().unwrap()))
            {
                let mut this_list_resolved_list: HashSet<ResolvedSelector> = HashSet::new();

                let mut selector_list_mapped: Vec<String> = selector_list
                    .children()
                    .into_iter()
                    .filter_map(|child| {
                        let selector_text = if let Some(selector) = AnyCssSelector::cast_ref(&child)
                        {
                            normalize_complex_selector(selector.clone())
                        } else {
                            child
                                .clone()
                                .cast::<AnyCssRelativeSelector>()
                                .unwrap()
                                .text()
                        };

                        if let Some(first) = this_list_resolved_list.get(&selector_text) {
                            output.push(DuplicateSelector {
                                first: first.selector_node.clone(),
                                duplicate: child.clone(),
                            });
                            return None;
                        }

                        this_list_resolved_list.insert(ResolvedSelector {
                            selector_text: selector_text.clone(),
                            selector_node: child,
                        });
                        Some(selector_text)
                    })
                    .collect();
                selector_list_mapped.sort();

                for r in resolve_nested_selectors(selector_list_mapped.join(","), rule) {
                    let split: Vec<&str> = r.split_whitespace().collect();
                    let normalized = split.join(" ").to_lowercase();
                    if let Some(first) = resolved_list.get(&normalized) {
                        output.push(DuplicateSelector {
                            first: first.selector_node.clone(),
                            duplicate: selector_list.clone(),
                        });
                    } else {
                        resolved_list.insert(ResolvedSelector {
                            selector_text: normalized.clone(),
                            selector_node: selector_list.clone().into(),
                        });
                    }
                }
            }
        }
        output
    }

    fn diagnostic(_: &RuleContext<Self>, node: &Self::State) -> Option<RuleDiagnostic> {
        //
        // Read our guidelines to write great diagnostics:
        // https://docs.rs/biome_analyze/latest/biome_analyze/#what-a-rule-should-say-to-the-user
        //
        let duplicate = node.duplicate.to_string();
        Some(
            RuleDiagnostic::new(
                rule_category!(),
                node.duplicate.text_range(),
                markup! {
                    "Duplicate selector \""<Emphasis>{duplicate}</Emphasis>"\","
                },
            )
            .detail(node.first.text_range(), "first occurence:"),
        )
    }
}

fn resolve_nested_selectors(selector: String, this_rule: CssSyntaxNode) -> Vec<String> {
    let mut parent_selectors: Vec<String> = vec![];
    let parent_rule = this_rule.parent().and_then(|parent| parent.grand_parent());

    match &parent_rule {
        None => return vec![selector],
        Some(parent_rule) => {
            if let Some(parent_rule) = AnyCssAtRule::cast_ref(&parent_rule) {
                let mut hasher = DefaultHasher::new();
                parent_rule.range().hash(&mut hasher);
                // Each @rule is unique scope
                // Use a hash to create the comparable scope
                parent_selectors.push(hasher.finish().to_string());
            }
            if let Some(parent_rule) = AnyCssRule::cast_ref(&parent_rule) {
                match parent_rule {
                    AnyCssRule::CssNestedQualifiedRule(parent_rule) => {
                        for selector in parent_rule.prelude() {
                            if let Ok(selector) = selector {
                                parent_selectors.push(selector.text());
                            }
                        }
                    }
                    AnyCssRule::CssQualifiedRule(parent_rule) => {
                        for selector in parent_rule.prelude() {
                            if let Ok(selector) = selector {
                                parent_selectors.push(selector.text());
                            }
                        }
                    }
                    _ => {
                        // Bogus rules are not handled
                        // AtRule is handled by AnyCssAtRule above
                    }
                }
            }

            let resolved_selectors: Vec<String> =
                parent_selectors
                    .iter()
                    .fold(vec![], |result: Vec<String>, parent_selector| {
                        if selector.contains("&") {
                            let resolved_parent_selectors = resolve_nested_selectors(
                                parent_selector.to_string(),
                                parent_rule.clone(),
                            );
                            let resolved = resolved_parent_selectors
                                .into_iter()
                                .map(|newly_resolved| return selector.replace("&", &newly_resolved))
                                .collect();
                            return [result, resolved].concat();
                        } else {
                            let combined_selectors = parent_selector.to_owned() + " " + &selector;
                            let resolved =
                                resolve_nested_selectors(combined_selectors, parent_rule.clone());
                            return [result, resolved].concat();
                        }
                    });
            if resolved_selectors.len() > 0 {
                return resolved_selectors;
            }
            return vec![selector];
        }
    }
}

fn normalize_complex_selector(selector: AnyCssSelector) -> String {
    let mut selector_text = String::new();

    if let Some(complex_selector) = CssComplexSelector::cast_ref(&selector.clone().into_syntax()) {
        if let Ok(left) = complex_selector.left() {
            selector_text.push_str(&left.text());
        }
        if let Ok(combinator) = complex_selector.combinator() {
            let combinator = combinator.text_trimmed();
            selector_text.push_str(combinator);
        }
        if let Ok(right) = complex_selector.right() {
            selector_text.push_str(&right.text());
        }
        return selector_text;
    }
    return selector.text();
}
