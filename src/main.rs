#![allow(unused_variables)]

use std::io::{stdin, stdout};
use crossterm::{
    execute,
    cursor::MoveTo,
    style::Print,
    terminal::{
        Clear,
        ClearType,
        EnterAlternateScreen,
        LeaveAlternateScreen
    }
};

use em_6502::{cpu::CPU6502, mem::Mem};

fn main() {
    let mut interactive = false;
    let mut args = std::env::args();
    if let Some(v) = args.nth(1) {
        match v.as_str() {
            "i" | "-i" | "--int" | "--interactive" => interactive = true,
            _ => {},
        }
    }

    if interactive {
        interactive_ui();
    } else {
        let mut cpu = CPU6502::new_with_mem_from_file("in_sample_2.txt".to_string()).unwrap();
        cpu.set_allow_hlt(true);
        cpu.set_illegal_opcode_mode(true);
        cpu.set_cycle_limit(0);

        cpu.power_on_and_run(true);

        println!("vvv CPU and Memory Postmortem vvv");
        println!("{:?}", cpu);
    }
}

fn interactive_ui() {
    _ = execute!(
        stdout(),
        EnterAlternateScreen,
        Clear(ClearType::All),
        MoveTo(0, 0)
    );

    let mut exit = false;
    let options = vec![
        "Exit",
        "Create a new CPU",
        "Interactively modify current CPU",
        "Run current CPU in step mode",
        "Run current CPU continuously",
        "Log current CPU and memory state to file",
    ];
    let mut cpu = CPU6502::new();
    
    while !exit {
        _ = execute!(stdout(), Clear(ClearType::All), MoveTo(0, 0));
        let mut buf_in = String::new();

        _ = execute!(
            stdout(),
            Print(format!("Current CPU state:\n\n{}\n\n", cpu)),
            Print("Please select an option:\n")
        );

        for (i, &opt) in options.iter().enumerate() {
            _ = execute!(stdout(), Print(format!("{}: {}\n", i, opt)));
        }
        
        _ = stdin().read_line(&mut buf_in);
        
        match buf_in.trim().parse::<usize>() {
            Ok(0) => exit = true,
            Ok(1) => cpu = new_cpu(),
            Ok(2) => edit_cpu(&mut cpu),
            Ok(3) => run_cpu_steps(),
            Ok(4) => run_cpu_continuous(),
            Ok(5) => print_to_file(&cpu),
            _ => _ = execute!(stdout(), Print(format!("{} is not a valid option\n", buf_in.trim()))),
        }
    }
    _ = execute!(std::io::stdout(), LeaveAlternateScreen);
}

fn new_cpu() -> CPU6502 {
    let mut valid = false;
    let mut buf_in = String::new();
    let cpu = CPU6502::new();

    let options = vec![
        "Finish",
        "Return new default CPU",
        "Return new CPU with memory all NOP",
        "Return new CPU with memory all HLT",
        "Return new CPU with memory from specification file",
    ];
    
    while !valid {
        _ = execute!(stdout(),
            Clear(ClearType::All),
            MoveTo(0, 0),
            Print(format!("Current CPU state:\n\n{}\n\n", cpu))
        );

        valid = true;
        buf_in.clear();

        _ = execute!(stdout(), Print("Select an option:\n"));
        for (i, &opt) in options.iter().enumerate() {
            _ = execute!(stdout(), Print(format!("{}: {}\n", i, opt)));
        }
        _ = stdin().read_line(&mut buf_in);

        match buf_in.trim().parse::<usize>() {
            Ok(0) => {},
            Ok(1) => return CPU6502::new(),
            Ok(2) => return CPU6502::new_with_mem(Mem::new_nops()),
            Ok(3) => return CPU6502::new_with_mem(Mem::new_hlts()),
            Ok(4) => {
                buf_in.clear();

                _ = execute!(stdout(), Print("Please input the file name: "));
                _ = stdin().read_line(&mut buf_in);

                if let Ok(mem) = Mem::new_from_file(buf_in.trim().to_string()) {
                    return CPU6502::new_with_mem(mem);
                } else {
                    valid = false;
                    _ = execute!(stdout(), Print(format!("Error loading memory from spec file \"{}\"", buf_in)));
                }
            },
            _ => {
                valid = false;
                _ = execute!(stdout(), Print(format!("{} is not a valid option\n", buf_in.trim())));
            }
        }

    }

    cpu
}

fn edit_cpu(cpu: &mut CPU6502) {
    // todo
}

fn run_cpu_steps() {
    // todo
}

fn run_cpu_continuous() {
    // todo
}

fn print_to_file(cpu: &CPU6502) {
    let mut buf_in = String::new();

    loop { 
        buf_in.clear();
        _ = execute!(stdout(), Print("Please specify file to output to, or type \"cancel\" to cancel: "));
        _ = stdin().read_line(&mut buf_in);

        if &buf_in.trim() == &"cancel" {
            break;
        } else if let Ok(_) = std::fs::write(&buf_in.trim(), format!("{:?}", cpu)) {
            break;
        } else {
            _ = execute!(stdout(), Print(format!("Error writing to file \"{}\"\n", &buf_in)));
        }
    }
}