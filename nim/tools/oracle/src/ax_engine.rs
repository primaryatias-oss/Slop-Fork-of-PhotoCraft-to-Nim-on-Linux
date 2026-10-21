//! Oracle commands for the Nim port of the engine (`crates/engine`, see nim/PORTING.md).
//!
//! `engine.specs` lists every registered command (id, label, menu, shortcut, params, journal) as
//! JSON lines; `engine.script SCRIPT.json OUTDIR` runs a list of `{"cmd", "params"}` steps in a
//! fresh session and writes each step's result or error, then the open documents as `.pcraft`.

use photocraft_engine::{Session, command_specs};
use serde_json::{Value, json};

pub fn run(args: &[String]) -> bool {
    match args[1].as_str() {
        "engine.specs" => {
            for s in command_specs() {
                println!("{}", json!({"id": s.id, "label": s.label, "menu": s.menu, "shortcut": s.shortcut, "params": s.params, "journal": s.journal}));
            }
        }
        "engine.script" => {
            let steps: Vec<Value> = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
            let out = std::path::Path::new(&args[3]);
            std::fs::create_dir_all(out).unwrap();
            std::panic::set_hook(Box::new(|_| {}));
            let mut s = Session::new();
            let mut log = Vec::new();
            for step in steps {
                let cmd = step["cmd"].as_str().unwrap_or("").to_string();
                let params = step.get("params").cloned().unwrap_or(Value::Null);
                let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| s.execute(&cmd, params)));
                log.push(match r {
                    Ok(Ok(v)) => json!({"ok": v}),
                    Ok(Err(e)) => json!({"err": e.to_string()}),
                    Err(_) => json!({"panic": true}),
                });
            }
            std::fs::write(out.join("log.json"), serde_json::to_string_pretty(&log).unwrap()).unwrap();
            for (i, d) in s.documents().iter().enumerate() {
                let b = photocraft_format::save_to_bytes(&d.doc, &Default::default()).unwrap();
                std::fs::write(out.join(format!("doc{i}.pcraft")), b).unwrap();
            }
        }
        _ => return false,
    }
    true
}
