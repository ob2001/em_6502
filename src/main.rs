#![allow(unused_variables)]

use std::io;
use crossterm::{
    ExecutableCommand,
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
    if let Some(mode) = std::env::args().nth(1) {
        match mode.as_str() {
            "-d" | "--dbg" => {
                let mut cpu = CPU6502::new_with_mem_from_file("in_sample_2.txt".to_string()).unwrap();
                cpu.set_allow_hlt(true);
                cpu.set_illegal_opcode_mode(true);
                cpu.set_cycle_limit(0);

                cpu.power_on_and_run(true);

                if false {
                    println!("vvv CPU and Memory Postmortem vvv");
                    println!("{:?}", cpu);
                }
            },
            "-i" | "--int" => {
                _ = interactive();
            },
            "-h" | "--help" => {
                print_help();
                return
            }
            _ => panic!("Incorrect usage - invalid program mode \"{}\"\nUse [-h] flag for usage instructions", mode),
        }
    } else {
        panic!("Incorrect usage - please specify a program mode\nUse [-h] flag for usage instructions");
    }
}

fn print_help() {
    let line_width = 46;
    println!("{:-^46}", "Usage");
    println!(" > em_6502 [-d/--dbg | -i/--int | -h/--help]");
    println!();
    print_left_centre_pad_right("[-d/--dbg]:", " ", "Debug mode. Used in development.", line_width);
    print_left_centre_pad_right("[-i/--int]:", " ", "Interactive mode. For general use.", line_width);
    println!();
    print_left_centre_pad_right("[-h/--help]:", " ", "Display this help menu.", line_width);
    println!("{:-^46}", "");
}

fn print_left_centre_pad_right(left: &str, pad: &str, right: &str, width: usize) {
    println!("{}{}{}", left, str::repeat(pad, width - left.len() - right.len()), right);
}

fn interactive() -> std::io::Result<()> {
    let mut stdout = io::stdout();
    
    stdout.execute(EnterAlternateScreen)?
    .execute(Clear(ClearType::All))?
    .execute(MoveTo(0, 0))?;

    let mut exit = false;
    let options = vec![
        "Exit",
        "Create a new CPU",
        "Interactively modify current CPU",
        "Run current CPU in step mode",
        "Run current CPU continuously",
        "Log current CPU and memory state to file",
    ];
    let mut cpu = CPU6502::default();
    
    while !exit {
        stdout.execute(Clear(ClearType::All))?
        .execute(MoveTo(0, 0))?;

        let mut buf_in = String::new();

        stdout.execute(Print(format!("Current CPU state:\n\n{}\n\n", cpu)))?
        .execute(Print("Please select an option:\n"))?;

        for (i, &opt) in options.iter().enumerate() {
            stdout.execute(Print(format!("{}: {}\n", i, opt)))?;
        }
        
        _ = io::stdin().read_line(&mut buf_in);
        
        match buf_in.trim().parse::<usize>() {
            Ok(0) => exit = true,
            Ok(1) => cpu = new_cpu().unwrap(),
            Ok(2) => edit_cpu(&mut cpu),
            Ok(3) => run_cpu_steps(&mut cpu),
            Ok(4) => run_cpu_continuous(&mut cpu),
            Ok(5) => print_to_file(&cpu).unwrap(),
            _ => { stdout.execute(Print(format!("{} is not a valid option\n", buf_in.trim())))?; },
        }
    }
    stdout.execute(LeaveAlternateScreen)?;
    Ok(())
}

fn new_cpu() -> io::Result<CPU6502> {
    let mut stdout = io::stdout();
    let mut valid = false;
    let mut buf_in = String::new();
    let cpu = CPU6502::default();

    let options = vec![
        "Finish",
        "Return new default CPU",
        "Return new CPU with memory all NOP",
        "Return new CPU with memory all HLT",
        "Return new CPU with memory from specification file",
    ];
    
    while !valid {
        stdout.execute(Clear(ClearType::All))?
        .execute(MoveTo(0, 0))?
        .execute(Print(format!("Current CPU state:\n\n{}\n\n", cpu)))?;

        valid = true;
        buf_in.clear();

        stdout.execute(Print("Select an option:\n"))?;
        for (i, &opt) in options.iter().enumerate() {
            stdout.execute(Print(format!("{}: {}\n", i, opt)))?;
        }
        _ = io::stdin().read_line(&mut buf_in);

        match buf_in.trim().parse::<usize>() {
            Ok(0) => {},
            Ok(1) => return Ok(CPU6502::default()),
            Ok(2) => return Ok(CPU6502::new_with_mem(Mem::new_nops())),
            Ok(3) => return Ok(CPU6502::new_with_mem(Mem::new_hlts())),
            Ok(4) => {
                buf_in.clear();

                stdout.execute(Print("Please input the file name: "))?;
                _ = io::stdin().read_line(&mut buf_in);

                if let Ok(mem) = Mem::new_from_file(buf_in.trim().to_string()) {
                    return Ok(CPU6502::new_with_mem(mem));
                } else {
                    valid = false;
                    stdout.execute(Print(format!("Error loading memory from spec file \"{}\"", buf_in)))?;
                }
            },
            _ => {
                valid = false;
                stdout.execute(Print(format!("{} is not a valid option\n", buf_in.trim())))?;
            }
        }
    }

    Ok(cpu)
}

fn edit_cpu(cpu: &mut CPU6502) {
    // todo
    todo!("interactive cpu editing to be implemented");
}

fn run_cpu_steps(cpu: &mut CPU6502) {
    // todo
    todo!("running CPU in step mode to be implemented");
}

fn run_cpu_continuous(cpu: &mut CPU6502) {
    // todo
    todo!("running CPU in continuous mode to be implemented")
}

fn print_to_file(cpu: &CPU6502) -> io::Result<()> {
    let mut stdout = io::stdout();
    let mut buf_in = String::new();

    loop { 
        buf_in.clear();
        stdout.execute(Print("Please specify file to output to, or type \"cancel\" to cancel: "))?;
        _ = io::stdin().read_line(&mut buf_in);

        if buf_in.trim() == "cancel" {
            break Ok(());
        } else if let Ok(_) = std::fs::write(&buf_in.trim(), format!("{:?}", cpu)) {
            break Ok(());
        } else {
            stdout.execute(Print(format!("Error writing to file \"{}\"\n", &buf_in)))?;
        }
    }
}