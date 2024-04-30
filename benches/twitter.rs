use std::{
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use json_parse::parse::JsonParser;

fn bench_json_parse(path: &Path) {
    let f = std::fs::File::open(path).unwrap();
    let r = std::io::BufReader::new(f);
    let mut p = JsonParser::new(r);
    let x = p.parse();
    black_box(x);
    black_box(p);
}

fn bench_json_parse_no_streaming(path: &Path) {
    let src = std::fs::read_to_string(path).unwrap();
    let mut p = JsonParser::new(Arc::from(src.into_bytes()));
    let x = p.parse();
    black_box(x);
    black_box(p);
}

fn bench_json(path: &Path) {
    let src = std::fs::read_to_string(path).unwrap();
    let x = json::parse(&src).unwrap();
    black_box(x);
}

fn bench_serde_json(path: &Path) {
    let f = std::fs::File::open(path).unwrap();
    let r = std::io::BufReader::new(f);
    let x: serde_json::Value = serde_json::from_reader(r).unwrap();
    black_box(x);
}

fn bench_serde_json_no_streaming(path: &Path) {
    let src = std::fs::read_to_string(path).unwrap();
    let x: serde_json::Value = serde_json::from_str(&src).unwrap();
    black_box(x);
}

pub fn entry(c: &mut Criterion) {
    let path = black_box(PathBuf::from("./twitter.json"));

    let mut grp = c.benchmark_group("Parse Twitter File");
    grp.bench_function("json_parse", |b| b.iter(|| bench_json_parse(&path)));
    grp.bench_function("json_parse_no_streaming", |b| {
        b.iter(|| bench_json_parse_no_streaming(&path))
    });
    grp.bench_function("json", |b| b.iter(|| bench_json(&path)));
    grp.bench_function("serde_json", |b| b.iter(|| bench_serde_json(&path)));
    grp.bench_function("serde_json_no_streaming", |b| {
        b.iter(|| bench_serde_json_no_streaming(&path))
    });
}

criterion_group!(benches, entry);
criterion_main!(benches);
