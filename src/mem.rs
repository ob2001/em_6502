use crate::prelude::*;

pub type Mem = [CPUByte; CPU_MEMSIZE];
pub type MemResult = Result<Mem, String>;

/// Create new cpu memory with all bytes set to the provided value
pub fn new_all(b: CPUByte) -> Mem {
    [b; CPU_MEMSIZE]
}

/// Create new cpu memory with all NOP instructions
pub fn new_nops() -> Mem {
    new_all(0x00)
}

/// Create new cpu memory with all (illegal) HLT instructions
pub fn new_hlts() -> Mem {
    new_all(0xFF)
}

/// Return new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
pub fn new_nops_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds".to_string())
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index".to_string())
    } else {
        let mut ret = new_nops();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

pub fn new_hlts_with_code_at(code: &[CPUByte], idx: usize) -> MemResult {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds".to_string())
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index".to_string())
    } else {
        let mut ret = new_hlts();

        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }

        Ok(ret)
    }
}

/// Generate CPU memory from a file specification
pub fn new_from_file(file: String) -> MemResult {
    if let Ok(f) = std::fs::read_to_string(&file) {
        let mut ret = new_nops();
        let mut byte_radix_mode: u32 = 16;

        for (i, line) in f.lines().enumerate() {
            let trim_line = line.trim();
            if trim_line.len() == 0 || trim_line.chars().next() == Some(';') { continue }
            // Line is now guaranteed to contain at least one non-comment, non-whitespace character.

            let trim_line = line.trim().split(';').next().expect(&format!("Parsing error: should be able to split line {} on \';\'", i + 1)).trim();
            // Comments are now guaranteed to be discarded and the non-comment
            // section of the line is trimmed of whitepace characters

            if trim_line.to_owned().chars().fold(0, |acc, c| acc + if c == ':' {1} else {0}) != 1 {
                return Err(format!("Parsing error - line {} should contain exactly one colon (:)", i + 1))
            }
            // Line is now guaranteed to contain exactly one colon (:)

            let mut split = trim_line.split(':').map(str::to_owned);
            let (pre, post) = (split.next().unwrap(), split.next().unwrap());
            let mut post = post.split(',').map(str::trim).map(str::to_owned);
            // pre is now our predicate that we match on to determine how to process post

            match pre.trim() {
                "byte_radix_mode" => {
                    let val = post.next().unwrap();
                    byte_radix_mode = match val.to_lowercase().as_str() {
                        "2" | "bin" | "binary" => 2,
                        "10" | "dec" | "decimal" => 10,
                        "16" | "hex" | "hexadecimal" => 16,
                        _ => panic!("Error parsing byte_radix_mode in line {}. Only support binary, decimal, or hexadecimal modes.", i + 1),
                    }
                },
                "fill_byte" => {
                    let fill_byte = &post.next().unwrap();
                    let fill_byte = u8::from_str_radix(fill_byte, byte_radix_mode).expect(&format!("Error parsing fill_byte in line {}: \"{}\" in an invalid base-{byte_radix_mode} representation of a byte", i + 1, fill_byte));
                    ret = new_all(fill_byte);
                },
                addr => {
                    let address = u16::from_str_radix(addr, 16).expect(&format!("Error parsing address predicate in line {}: \"{}\" is an invalid hexadecimal representation of 2 bytes", i + 1, addr));
                    let mut mem_line: Vec<u8> = Vec::new();
                    
                    for byte in post {
                        let byte = byte.trim();
                        let byte = u8::from_str_radix(byte, byte_radix_mode).expect(&format!("Error parsing byte in line {}: \"{}\" is an invalid base-{byte_radix_mode} representation of a byte", i + 1, byte));
                        mem_line.push(byte);
                    }

                    insert_code_at(&mut ret, &mem_line, address as usize)?;
                }
            }
        }

        Ok(ret)
    } else {
        Err(format!("Error reading supplied memory specification file: {}", file))
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
    if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else if code.len() + idx > CPU_MEMSIZE {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    } else {
        for (offset, &byte) in code.iter().enumerate() {
            mem[idx + offset] = byte;
        }
        Ok(())
    }
}

