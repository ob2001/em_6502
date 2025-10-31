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
    if let Ok(f) = std::fs::read_to_string(file) {
        let mut ret = new_nops();
        let mut radix_mode: u32 = 16;

        for (i, line) in f.lines().enumerate() {
            if line.len() == 0 { continue }

            let trim_line = line.trim().split(';').next().expect(&format!("Parsing error: should be able to split line {i} on \';\'")).trim();
            if !trim_line.contains(':') { return Err(format!("Parsing error - line {} is missing a colon (:)", i + 1)) }

            let split: Vec<String> = trim_line.split(':').map(str::to_owned).collect();
            if split.len() != 2 { return Err(format!("Error splitting on colon (:): line {} contains too many colons (:)", i + 1)) }

            let (pre, post) = (split[0].clone(), split[1].clone());

            let post: Vec<String> = post.split(',').map(str::trim).map(str::to_owned).collect();

            match pre.trim() {
                "radix_mode" => {
                    let mode = u32::from_str_radix(&post[0], 10).expect(&format!("Error parsing radix_mode in line {}: \"{}\" is not a decimal literal", i + 1, &post[0]));
                    radix_mode = mode;
                },
                "pad_all" => {
                    let pad_byte = u8::from_str_radix(&post[0], radix_mode).expect(&format!("Error parsing pad_byte in line {}: \"{}\" in an invalid base-{radix_mode} byte representation", i + 1, &post[0]));
                    ret = new_all(pad_byte);
                },
                addr => {
                    let address = u16::from_str_radix(addr, radix_mode).expect(&format!("Error parsing address in line {}: \"{}\" is an invalid base-{radix_mode} 2-byte representation", i + 1, addr));
                    let mut mem_line: Vec<u8> = Vec::new();
                    
                    for byte in post.iter().map(|b| b.trim()) {
                        let byte = u8::from_str_radix(byte, radix_mode).expect(&format!("Error parsing byte in line {}: \"{}\" is an invalid base-{radix_mode} byte representation", i + 1, byte));
                        mem_line.push(byte);
                    }

                    insert_code_at(&mut ret, &mem_line, address as usize)?;
                }
            }
        }

        Ok(ret)
    } else {
        MemResult::Err("".to_string())
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            mem[idx + offset] = byte;
        }
        Ok(())
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

