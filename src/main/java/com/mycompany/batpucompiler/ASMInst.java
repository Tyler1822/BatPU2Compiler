/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.batpucompiler;

/**
 *
 * @author tyler
 */
public class ASMInst {
    /* ISA can be found @ https://docs.google.com/spreadsheets/d/1Bj3wHV-JifR2vP4HRYoCWrdXYp3sGMG0Q58Nm56W4aI/edit?gid=0#gid=0
    Credit: Matbattwings on youtube https://www.youtube.com/@mattbatwings
    */
    InstType type;
    
    // register values (max value 16)
    int rd = -1; // destitation
    int ra = -1; // source a
    int rb = -1; // srouce b
    
    int imm = 0; // immediate (offset for memory inst)
    int cond = -1; // branch condition
    int addr = -1; // jump or branch address destiation
    
    int pc; // instruction idx
    
    String label; // used for functions
    
    public ASMInst(InstType type) {
        this.type = type;
    }
    
    static ASMInst reg3(InstType t, int ra, int rb, int rd) { // used for most ALU instructions
        ASMInst i = new ASMInst(t);
        i.ra = ra;
        i.rb = rb;
        i.rd = rd;
        return i;
    }

    static ASMInst regImm(InstType t, int rd, int imm) { // used for immidates
        ASMInst i = new ASMInst(t);
        i.rd = rd;
        i.imm = imm;
        return i;
    }

    static ASMInst branch(int cond, int addr) { // used for BRH
        ASMInst i = new ASMInst(InstType.BRH);
        i.cond = cond;
        i.addr = addr;
        return i;
    }
    
    static ASMInst jump(int addr) { // used for JMP
        ASMInst i = new ASMInst(InstType.JMP);
        i.addr = addr;
        return i;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        // optional PC prefix
        //sb.append(String.format("%04d: ", pc));

        sb.append(type);

        // 3-register ALU instructions
        if (ra != -1 || rb != -1 || rd != -1) {
            if (ra != -1) sb.append(" r").append(ra);
            if (rb != -1) sb.append(" r").append(rb);
            if(this.type != InstType.STR) { // 3rd opperand of STR is a 4 bit signed literal
                if (rd != -1) sb.append(" r").append(rd);
            } else {
                sb.append(" ").append(rd);
            }
            
        }

        // immediate
        if (imm != 0 || (type == InstType.LDI)) {
            sb.append(" ").append(imm);
        }

        // branch condition
        if (cond != -1) {
            sb.append(" ").append(cond);
        }

        // jump / branch target
        if (addr != -1) {
            sb.append(" ").append(addr);
        }

        return sb.toString();
    }

}
