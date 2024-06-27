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
            {
                "text": "@aym0566x \n\n名前:前田あゆみ\n第一印象:なんか怖っ！\n今の印象:とりあえずキモい。噛み合わない\n好きなところ:ぶすでキモいとこ😋✨✨\n思い出:んーーー、ありすぎ😊❤️\nLINE交換できる？:あぁ……ごめん✋\nトプ画をみて:照れますがな😘✨\n一言:お前は一生もんのダチ💖",
                "source": "<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>"
            },
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
    let raw = include_str!("../twitter.json");

    let p = parse(raw.as_bytes());
    // println!("{p}");
}
