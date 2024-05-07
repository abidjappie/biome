pub const BASIC_KEYWORDS: [&str; 5] = ["initial", "inherit", "revert", "revert-layer", "unset"];

// https://drafts.csswg.org/css-fonts/#system-family-name-value
pub const SYSTEM_FAMILY_NAME_KEYWORDS: [&str; 6] = [
    "caption",
    "icon",
    "menu",
    "message-box",
    "small-caption",
    "status-bar",
];

pub const FONT_FAMILY_KEYWORDS: [&str; 10] = [
    "serif",
    "sans-serif",
    "cursive",
    "fantasy",
    "monospace",
    "system-ui",
    "ui-serif",
    "ui-sans-serif",
    "ui-monospace",
    "ui-rounded",
];

pub const FONT_WEIGHT_ABSOLUTE_KEYWORDS: [&str; 2] = ["normal", "bold"];
pub const FONT_WEIGHT_NUMERIC_KEYWORDS: [&str; 9] = [
    "100", "200", "300", "400", "500", "600", "700", "800", "900",
];
pub const FONT_STYLE_KEYWORDS: [&str; 3] = ["normal", "italic", "oblique"];
pub const FONT_VARIANTS_KEYWORDS: [&str; 35] = [
    "normal",
    "none",
    "historical-forms",
    "none",
    "common-ligatures",
    "no-common-ligatures",
    "discretionary-ligatures",
    "no-discretionary-ligatures",
    "historical-ligatures",
    "no-historical-ligatures",
    "contextual",
    "no-contextual",
    "small-caps",
    "all-small-caps",
    "petite-caps",
    "all-petite-caps",
    "unicase",
    "titling-caps",
    "lining-nums",
    "oldstyle-nums",
    "proportional-nums",
    "tabular-nums",
    "diagonal-fractions",
    "stacked-fractions",
    "ordinal",
    "slashed-zero",
    "jis78",
    "jis83",
    "jis90",
    "jis04",
    "simplified",
    "traditional",
    "full-width",
    "proportional-width",
    "ruby",
];

pub const FONT_STRETCH_KEYWORDS: [&str; 8] = [
    "semi-condensed",
    "condensed",
    "extra-condensed",
    "ultra-condensed",
    "semi-expanded",
    "expanded",
    "extra-expanded",
    "ultra-expanded",
];

pub const FONT_SIZE_KEYWORDS: [&str; 9] = [
    "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "larger", "smaller",
];

pub const LINE_HEIGHT_KEYWORDS: [&str; 1] = ["normal"];

/// List of known CSS value functions sourced from [`css-functions-list`](https://www.npmjs.com/package/css-functions-list).
/// See the original list [here](https://github.com/niksy/css-functions-list/blob/master/index.json).
pub const FUNCTION_KEYWORDS: [&str; 671] = [
    "-moz-abs",
    "-moz-acos",
    "-moz-anchor",
    "-moz-anchor-size",
    "-moz-annotation",
    "-moz-asin",
    "-moz-atan",
    "-moz-atan2",
    "-moz-attr",
    "-moz-blur",
    "-moz-brightness",
    "-moz-calc",
    "-moz-character-variant",
    "-moz-circle",
    "-moz-clamp",
    "-moz-color",
    "-moz-color-contrast",
    "-moz-color-mix",
    "-moz-color-stop",
    "-moz-conic-gradient",
    "-moz-contrast",
    "-moz-cos",
    "-moz-counter",
    "-moz-counters",
    "-moz-cross-fade",
    "-moz-cubic-bezier",
    "-moz-device-cmyk",
    "-moz-drop-shadow",
    "-moz-element",
    "-moz-ellipse",
    "-moz-env",
    "-moz-exp",
    "-moz-fit-content",
    "-moz-format",
    "-moz-from",
    "-moz-gradient",
    "-moz-grayscale",
    "-moz-hsl",
    "-moz-hsla",
    "-moz-hue-rotate",
    "-moz-hwb",
    "-moz-hypot",
    "-moz-image",
    "-moz-image-rect",
    "-moz-image-set",
    "-moz-inset",
    "-moz-invert",
    "-moz-lab",
    "-moz-layer",
    "-moz-lch",
    "-moz-leader",
    "-moz-light-dark",
    "-moz-linear",
    "-moz-linear-gradient",
    "-moz-local",
    "-moz-log",
    "-moz-matrix",
    "-moz-matrix3d",
    "-moz-max",
    "-moz-min",
    "-moz-minmax",
    "-moz-mod",
    "-moz-oklab",
    "-moz-oklch",
    "-moz-opacity",
    "-moz-ornaments",
    "-moz-paint",
    "-moz-palette-mix",
    "-moz-path",
    "-moz-perspective",
    "-moz-polygon",
    "-moz-pow",
    "-moz-radial-gradient",
    "-moz-ray",
    "-moz-rect",
    "-moz-rem",
    "-moz-repeat",
    "-moz-repeating-conic-gradient",
    "-moz-repeating-linear-gradient",
    "-moz-repeating-radial-gradient",
    "-moz-reversed",
    "-moz-rgb",
    "-moz-rgba",
    "-moz-rotate",
    "-moz-rotate3d",
    "-moz-rotateX",
    "-moz-rotateY",
    "-moz-rotateZ",
    "-moz-rotatex",
    "-moz-rotatey",
    "-moz-rotatez",
    "-moz-round",
    "-moz-saturate",
    "-moz-scale",
    "-moz-scale3d",
    "-moz-scaleX",
    "-moz-scaleY",
    "-moz-scaleZ",
    "-moz-scalex",
    "-moz-scaley",
    "-moz-scalez",
    "-moz-scroll",
    "-moz-selector",
    "-moz-sepia",
    "-moz-sign",
    "-moz-sin",
    "-moz-skew",
    "-moz-skewX",
    "-moz-skewY",
    "-moz-skewx",
    "-moz-skewy",
    "-moz-sqrt",
    "-moz-steps",
    "-moz-styleset",
    "-moz-stylistic",
    "-moz-swash",
    "-moz-symbols",
    "-moz-tan",
    "-moz-target-counter",
    "-moz-target-counters",
    "-moz-target-text",
    "-moz-to",
    "-moz-translate",
    "-moz-translate3d",
    "-moz-translateX",
    "-moz-translateY",
    "-moz-translateZ",
    "-moz-translatex",
    "-moz-translatey",
    "-moz-translatez",
    "-moz-type",
    "-moz-url",
    "-moz-var",
    "-moz-view",
    "-moz-xywh",
    "-ms-abs",
    "-ms-acos",
    "-ms-anchor",
    "-ms-anchor-size",
    "-ms-annotation",
    "-ms-asin",
    "-ms-atan",
    "-ms-atan2",
    "-ms-attr",
    "-ms-blur",
    "-ms-brightness",
    "-ms-calc",
    "-ms-character-variant",
    "-ms-circle",
    "-ms-clamp",
    "-ms-color",
    "-ms-color-contrast",
    "-ms-color-mix",
    "-ms-color-stop",
    "-ms-conic-gradient",
    "-ms-contrast",
    "-ms-cos",
    "-ms-counter",
    "-ms-counters",
    "-ms-cross-fade",
    "-ms-cubic-bezier",
    "-ms-device-cmyk",
    "-ms-drop-shadow",
    "-ms-element",
    "-ms-ellipse",
    "-ms-env",
    "-ms-exp",
    "-ms-fit-content",
    "-ms-format",
    "-ms-from",
    "-ms-gradient",
    "-ms-grayscale",
    "-ms-hsl",
    "-ms-hsla",
    "-ms-hue-rotate",
    "-ms-hwb",
    "-ms-hypot",
    "-ms-image",
    "-ms-image-set",
    "-ms-inset",
    "-ms-invert",
    "-ms-lab",
    "-ms-layer",
    "-ms-lch",
    "-ms-leader",
    "-ms-light-dark",
    "-ms-linear",
    "-ms-linear-gradient",
    "-ms-local",
    "-ms-log",
    "-ms-matrix",
    "-ms-matrix3d",
    "-ms-max",
    "-ms-min",
    "-ms-minmax",
    "-ms-mod",
    "-ms-oklab",
    "-ms-oklch",
    "-ms-opacity",
    "-ms-ornaments",
    "-ms-paint",
    "-ms-palette-mix",
    "-ms-path",
    "-ms-perspective",
    "-ms-polygon",
    "-ms-pow",
    "-ms-radial-gradient",
    "-ms-ray",
    "-ms-rect",
    "-ms-rem",
    "-ms-repeat",
    "-ms-repeating-conic-gradient",
    "-ms-repeating-linear-gradient",
    "-ms-repeating-radial-gradient",
    "-ms-reversed",
    "-ms-rgb",
    "-ms-rgba",
    "-ms-rotate",
    "-ms-rotate3d",
    "-ms-rotateX",
    "-ms-rotateY",
    "-ms-rotateZ",
    "-ms-rotatex",
    "-ms-rotatey",
    "-ms-rotatez",
    "-ms-round",
    "-ms-saturate",
    "-ms-scale",
    "-ms-scale3d",
    "-ms-scaleX",
    "-ms-scaleY",
    "-ms-scaleZ",
    "-ms-scalex",
    "-ms-scaley",
    "-ms-scalez",
    "-ms-scroll",
    "-ms-selector",
    "-ms-sepia",
    "-ms-sign",
    "-ms-sin",
    "-ms-skew",
    "-ms-skewX",
    "-ms-skewY",
    "-ms-skewx",
    "-ms-skewy",
    "-ms-sqrt",
    "-ms-steps",
    "-ms-styleset",
    "-ms-stylistic",
    "-ms-swash",
    "-ms-symbols",
    "-ms-tan",
    "-ms-target-counter",
    "-ms-target-counters",
    "-ms-target-text",
    "-ms-to",
    "-ms-translate",
    "-ms-translate3d",
    "-ms-translateX",
    "-ms-translateY",
    "-ms-translateZ",
    "-ms-translatex",
    "-ms-translatey",
    "-ms-translatez",
    "-ms-type",
    "-ms-url",
    "-ms-var",
    "-ms-view",
    "-ms-xywh",
    "-o-abs",
    "-o-acos",
    "-o-anchor",
    "-o-anchor-size",
    "-o-annotation",
    "-o-asin",
    "-o-atan",
    "-o-atan2",
    "-o-attr",
    "-o-blur",
    "-o-brightness",
    "-o-calc",
    "-o-character-variant",
    "-o-circle",
    "-o-clamp",
    "-o-color",
    "-o-color-contrast",
    "-o-color-mix",
    "-o-color-stop",
    "-o-conic-gradient",
    "-o-contrast",
    "-o-cos",
    "-o-counter",
    "-o-counters",
    "-o-cross-fade",
    "-o-cubic-bezier",
    "-o-device-cmyk",
    "-o-drop-shadow",
    "-o-element",
    "-o-ellipse",
    "-o-env",
    "-o-exp",
    "-o-fit-content",
    "-o-format",
    "-o-from",
    "-o-gradient",
    "-o-grayscale",
    "-o-hsl",
    "-o-hsla",
    "-o-hue-rotate",
    "-o-hwb",
    "-o-hypot",
    "-o-image",
    "-o-image-set",
    "-o-inset",
    "-o-invert",
    "-o-lab",
    "-o-layer",
    "-o-lch",
    "-o-leader",
    "-o-light-dark",
    "-o-linear",
    "-o-linear-gradient",
    "-o-local",
    "-o-log",
    "-o-matrix",
    "-o-matrix3d",
    "-o-max",
    "-o-min",
    "-o-minmax",
    "-o-mod",
    "-o-oklab",
    "-o-oklch",
    "-o-opacity",
    "-o-ornaments",
    "-o-paint",
    "-o-palette-mix",
    "-o-path",
    "-o-perspective",
    "-o-polygon",
    "-o-pow",
    "-o-radial-gradient",
    "-o-ray",
    "-o-rect",
    "-o-rem",
    "-o-repeat",
    "-o-repeating-conic-gradient",
    "-o-repeating-linear-gradient",
    "-o-repeating-radial-gradient",
    "-o-reversed",
    "-o-rgb",
    "-o-rgba",
    "-o-rotate",
    "-o-rotate3d",
    "-o-rotateX",
    "-o-rotateY",
    "-o-rotateZ",
    "-o-rotatex",
    "-o-rotatey",
    "-o-rotatez",
    "-o-round",
    "-o-saturate",
    "-o-scale",
    "-o-scale3d",
    "-o-scaleX",
    "-o-scaleY",
    "-o-scaleZ",
    "-o-scalex",
    "-o-scaley",
    "-o-scalez",
    "-o-scroll",
    "-o-selector",
    "-o-sepia",
    "-o-sign",
    "-o-sin",
    "-o-skew",
    "-o-skewX",
    "-o-skewY",
    "-o-skewx",
    "-o-skewy",
    "-o-sqrt",
    "-o-steps",
    "-o-styleset",
    "-o-stylistic",
    "-o-swash",
    "-o-symbols",
    "-o-tan",
    "-o-target-counter",
    "-o-target-counters",
    "-o-target-text",
    "-o-to",
    "-o-translate",
    "-o-translate3d",
    "-o-translateX",
    "-o-translateY",
    "-o-translateZ",
    "-o-translatex",
    "-o-translatey",
    "-o-translatez",
    "-o-type",
    "-o-url",
    "-o-var",
    "-o-view",
    "-o-xywh",
    "-webkit-abs",
    "-webkit-acos",
    "-webkit-anchor",
    "-webkit-anchor-size",
    "-webkit-annotation",
    "-webkit-asin",
    "-webkit-atan",
    "-webkit-atan2",
    "-webkit-attr",
    "-webkit-blur",
    "-webkit-brightness",
    "-webkit-calc",
    "-webkit-character-variant",
    "-webkit-circle",
    "-webkit-clamp",
    "-webkit-color",
    "-webkit-color-contrast",
    "-webkit-color-mix",
    "-webkit-color-stop",
    "-webkit-conic-gradient",
    "-webkit-contrast",
    "-webkit-cos",
    "-webkit-counter",
    "-webkit-counters",
    "-webkit-cross-fade",
    "-webkit-cubic-bezier",
    "-webkit-device-cmyk",
    "-webkit-drop-shadow",
    "-webkit-element",
    "-webkit-ellipse",
    "-webkit-env",
    "-webkit-exp",
    "-webkit-fit-content",
    "-webkit-format",
    "-webkit-from",
    "-webkit-gradient",
    "-webkit-grayscale",
    "-webkit-hsl",
    "-webkit-hsla",
    "-webkit-hue-rotate",
    "-webkit-hwb",
    "-webkit-hypot",
    "-webkit-image",
    "-webkit-image-set",
    "-webkit-inset",
    "-webkit-invert",
    "-webkit-lab",
    "-webkit-layer",
    "-webkit-lch",
    "-webkit-leader",
    "-webkit-light-dark",
    "-webkit-linear",
    "-webkit-linear-gradient",
    "-webkit-local",
    "-webkit-log",
    "-webkit-matrix",
    "-webkit-matrix3d",
    "-webkit-max",
    "-webkit-min",
    "-webkit-minmax",
    "-webkit-mod",
    "-webkit-oklab",
    "-webkit-oklch",
    "-webkit-opacity",
    "-webkit-ornaments",
    "-webkit-paint",
    "-webkit-palette-mix",
    "-webkit-path",
    "-webkit-perspective",
    "-webkit-polygon",
    "-webkit-pow",
    "-webkit-radial-gradient",
    "-webkit-ray",
    "-webkit-rect",
    "-webkit-rem",
    "-webkit-repeat",
    "-webkit-repeating-conic-gradient",
    "-webkit-repeating-linear-gradient",
    "-webkit-repeating-radial-gradient",
    "-webkit-reversed",
    "-webkit-rgb",
    "-webkit-rgba",
    "-webkit-rotate",
    "-webkit-rotate3d",
    "-webkit-rotateX",
    "-webkit-rotateY",
    "-webkit-rotateZ",
    "-webkit-rotatex",
    "-webkit-rotatey",
    "-webkit-rotatez",
    "-webkit-round",
    "-webkit-saturate",
    "-webkit-scale",
    "-webkit-scale3d",
    "-webkit-scaleX",
    "-webkit-scaleY",
    "-webkit-scaleZ",
    "-webkit-scalex",
    "-webkit-scaley",
    "-webkit-scalez",
    "-webkit-scroll",
    "-webkit-selector",
    "-webkit-sepia",
    "-webkit-sign",
    "-webkit-sin",
    "-webkit-skew",
    "-webkit-skewX",
    "-webkit-skewY",
    "-webkit-skewx",
    "-webkit-skewy",
    "-webkit-sqrt",
    "-webkit-steps",
    "-webkit-styleset",
    "-webkit-stylistic",
    "-webkit-swash",
    "-webkit-symbols",
    "-webkit-tan",
    "-webkit-target-counter",
    "-webkit-target-counters",
    "-webkit-target-text",
    "-webkit-to",
    "-webkit-translate",
    "-webkit-translate3d",
    "-webkit-translateX",
    "-webkit-translateY",
    "-webkit-translateZ",
    "-webkit-translatex",
    "-webkit-translatey",
    "-webkit-translatez",
    "-webkit-type",
    "-webkit-url",
    "-webkit-var",
    "-webkit-view",
    "-webkit-xywh",
    "abs",
    "acos",
    "anchor",
    "anchor-size",
    "annotation",
    "asin",
    "atan",
    "atan2",
    "attr",
    "blur",
    "brightness",
    "calc",
    "character-variant",
    "circle",
    "clamp",
    "color",
    "color-contrast",
    "color-mix",
    "color-stop",
    "conic-gradient",
    "contrast",
    "cos",
    "counter",
    "counters",
    "cross-fade",
    "cubic-bezier",
    "device-cmyk",
    "drop-shadow",
    "element",
    "ellipse",
    "env",
    "exp",
    "fit-content",
    "format",
    "from",
    "gradient",
    "grayscale",
    "hsl",
    "hsla",
    "hue-rotate",
    "hwb",
    "hypot",
    "image",
    "image-set",
    "inset",
    "invert",
    "lab",
    "layer",
    "lch",
    "leader",
    "light-dark",
    "linear",
    "linear-gradient",
    "local",
    "log",
    "matrix",
    "matrix3d",
    "max",
    "min",
    "minmax",
    "mod",
    "oklab",
    "oklch",
    "opacity",
    "ornaments",
    "paint",
    "palette-mix",
    "path",
    "perspective",
    "polygon",
    "pow",
    "radial-gradient",
    "ray",
    "rect",
    "rem",
    "repeat",
    "repeating-conic-gradient",
    "repeating-linear-gradient",
    "repeating-radial-gradient",
    "reversed",
    "rgb",
    "rgba",
    "rotate",
    "rotate3d",
    "rotateX",
    "rotateY",
    "rotateZ",
    "rotatex",
    "rotatey",
    "rotatez",
    "round",
    "saturate",
    "scale",
    "scale3d",
    "scaleX",
    "scaleY",
    "scaleZ",
    "scalex",
    "scaley",
    "scalez",
    "scroll",
    "selector",
    "sepia",
    "sign",
    "sin",
    "skew",
    "skewX",
    "skewY",
    "skewx",
    "skewy",
    "sqrt",
    "steps",
    "styleset",
    "stylistic",
    "swash",
    "symbols",
    "tan",
    "target-counter",
    "target-counters",
    "target-text",
    "to",
    "translate",
    "translate3d",
    "translateX",
    "translateY",
    "translateZ",
    "translatex",
    "translatey",
    "translatez",
    "type",
    "url",
    "var",
    "view",
    "xywh",
];

// These are the ones that can have single-colon notation
pub const LEVEL_ONE_AND_TWO_PSEUDO_ELEMENTS: [&str; 4] =
    ["before", "after", "first-line", "first-letter"];

pub const VENDOR_SPECIFIC_PSEUDO_ELEMENTS: [&str; 66] = [
    "-moz-focus-inner",
    "-moz-focus-outer",
    "-moz-list-bullet",
    "-moz-meter-bar",
    "-moz-placeholder",
    "-moz-progress-bar",
    "-moz-range-progress",
    "-moz-range-thumb",
    "-moz-range-track",
    "-ms-browse",
    "-ms-check",
    "-ms-clear",
    "-ms-expand",
    "-ms-fill",
    "-ms-fill-lower",
    "-ms-fill-upper",
    "-ms-reveal",
    "-ms-thumb",
    "-ms-ticks-after",
    "-ms-ticks-before",
    "-ms-tooltip",
    "-ms-track",
    "-ms-value",
    "-webkit-color-swatch",
    "-webkit-color-swatch-wrapper",
    "-webkit-calendar-picker-indicator",
    "-webkit-clear-button",
    "-webkit-date-and-time-value",
    "-webkit-datetime-edit",
    "-webkit-datetime-edit-ampm-field",
    "-webkit-datetime-edit-day-field",
    "-webkit-datetime-edit-fields-wrapper",
    "-webkit-datetime-edit-hour-field",
    "-webkit-datetime-edit-millisecond-field",
    "-webkit-datetime-edit-minute-field",
    "-webkit-datetime-edit-month-field",
    "-webkit-datetime-edit-second-field",
    "-webkit-datetime-edit-text",
    "-webkit-datetime-edit-week-field",
    "-webkit-datetime-edit-year-field",
    "-webkit-details-marker",
    "-webkit-distributed",
    "-webkit-file-upload-button",
    "-webkit-input-placeholder",
    "-webkit-keygen-select",
    "-webkit-meter-bar",
    "-webkit-meter-even-less-good-value",
    "-webkit-meter-inner-element",
    "-webkit-meter-optimum-value",
    "-webkit-meter-suboptimum-value",
    "-webkit-progress-bar",
    "-webkit-progress-inner-element",
    "-webkit-progress-value",
    "-webkit-search-cancel-button",
    "-webkit-search-decoration",
    "-webkit-search-results-button",
    "-webkit-search-results-decoration",
    "-webkit-slider-runnable-track",
    "-webkit-slider-thumb",
    "-webkit-textfield-decoration-container",
    "-webkit-validation-bubble",
    "-webkit-validation-bubble-arrow",
    "-webkit-validation-bubble-arrow-clipper",
    "-webkit-validation-bubble-heading",
    "-webkit-validation-bubble-message",
    "-webkit-validation-bubble-text-block",
];

pub const SHADOW_TREE_PSEUDO_ELEMENTS: [&str; 1] = ["part"];

pub const OTHER_PSEUDO_ELEMENTS: [&str; 18] = [
    "backdrop",
    "content",
    "cue",
    "file-selector-button",
    "grammar-error",
    "highlight",
    "marker",
    "placeholder",
    "selection",
    "shadow",
    "slotted",
    "spelling-error",
    "target-text",
    "view-transition",
    "view-transition-group",
    "view-transition-image-pair",
    "view-transition-new",
    "view-transition-old",
];

pub const VENDER_PREFIXES: [&str; 4] = ["-webkit-", "-moz-", "-ms-", "-o-"];

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::FUNCTION_KEYWORDS;

    #[test]
    fn test_function_keywords_sorted() {
        let mut sorted = FUNCTION_KEYWORDS.to_vec();
        sorted.sort_unstable();
        assert_eq!(FUNCTION_KEYWORDS, sorted.as_slice());
    }

    #[test]
    fn test_function_keywords_unique() {
        let mut set = HashSet::new();
        let has_duplicates = FUNCTION_KEYWORDS.iter().any(|&x| !set.insert(x));
        assert!(!has_duplicates);
    }
}
