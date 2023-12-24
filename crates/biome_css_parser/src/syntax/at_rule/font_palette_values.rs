use crate::syntax::blocks::parse_or_recover_declaration_list_block;
use crate::{
    parser::CssParser,
    syntax::{parse_error::expected_identifier, parse_regular_identifier},
};
use biome_css_syntax::{
    CssSyntaxKind::{self, *},
    T,
};
use biome_parser::{
    parse_recovery::ParseRecovery,
    parsed_syntax::ParsedSyntax::{self, Present},
    prelude::ParsedSyntax::Absent,
    token_set, Parser, TokenSet,
};

#[inline]
pub(crate) fn is_at_font_palette_values_at_rule(p: &mut CssParser) -> bool {
    p.at(T![font_palette_values])
}

#[inline]
pub(crate) fn parse_font_palette_values_at_rule(p: &mut CssParser) -> ParsedSyntax {
    if !is_at_font_palette_values_at_rule(p) {
        return Absent;
    }

    let m = p.start();

    p.bump(T![font_palette_values]);

    let kind = if parse_regular_identifier(p)
        .or_recover(
            p,
            &ParseRecovery::new(CSS_BOGUS, FONT_PALETTE_VALUES_RECOVERY_SET)
                .enable_recovery_on_line_break(),
            expected_identifier,
        )
        .is_ok()
    {
        CSS_FONT_PALETTE_VALUES_AT_RULE
    } else {
        CSS_BOGUS_AT_RULE
    };

    if parse_or_recover_declaration_list_block(p).is_err() {
        return Present(m.complete(p, CSS_BOGUS_AT_RULE));
    }

    Present(m.complete(p, kind))
}

const FONT_PALETTE_VALUES_RECOVERY_SET: TokenSet<CssSyntaxKind> = token_set![T!['{']];
