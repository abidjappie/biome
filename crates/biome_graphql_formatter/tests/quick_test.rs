use biome_formatter::{IndentStyle, LineWidth};
use biome_formatter_test::check_reformat::CheckReformat;
use biome_graphql_formatter::context::GraphqlFormatOptions;
use biome_graphql_formatter::format_node;
use biome_graphql_parser::parse_graphql;

mod language {
    include!("language.rs");
}

#[ignore]
#[test]
// use this test check if your snippet prints as you wish, without using a snapshot
fn quick_test() {
    let src = r#"
type Artist implements Node & Entity {
  # The ID of an object
  id: ID!

  # The MBID of the entity.
  mbid: MBID!

  # A list of recordings linked to this entity.
  recordings(after: String, first: Int): RecordingConnection

  # A list of releases linked to this entity.
  releases(
    # Filter by one or more release group types.
    type: [ReleaseGroupType]

    # Filter by one or more release statuses.
    status: [ReleaseStatus]
    after: String
    first: Int
  ): ReleaseConnection

}

"#;
    let parse = parse_graphql(src);
    println!("{:#?}", parse);

    let options = GraphqlFormatOptions::default()
        .with_line_width(LineWidth::try_from(80).unwrap())
        .with_indent_style(IndentStyle::Space);
    let doc = format_node(options.clone(), &parse.syntax()).unwrap();
    let result = doc.print().unwrap();

    let root = &parse.syntax();
    let language = language::GraphqlTestFormatLanguage::default();

    println!("{}", doc.into_document());
    eprintln!("{}", result.as_code());

    CheckReformat::new(root, result.as_code(), "quick_test", &language, options).check_reformat();
}
