use std::sync::Arc;

use json_parse::parse::{JsonParser, ParseInput};

fn parse<I: ParseInput>(input: I) -> JsonParser<I> {
    let mut p = JsonParser::new(input);
    p.parse();
    p
}

fn main() {
    let raw = r#"

        [
            "Hello🌱 there, yall!",
            3.14,
            true,
            true,
            true,
            true,
            { "hi": true, "hello": null, "yo": null },
            "Hi\u2122 \"if you say so\" \\ again,,,",
            {},
            {
                "hello" : "my_val",
                "bye" : {
                    "foo" : 2,
                    "bar" : 2.99792458e8,
                    "also": 0.30
                }
            }
        ]

    "#;

    let p = parse(Arc::from(raw.as_bytes()));
    println!("{p}");
}
