/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Enum.java to edit this template
 */
package com.mycompany.batpucompiler;

/**
 *
 * @author tyler
 */
public enum InstType {
    NOP,
    HLT,
    ADD,
    SUB,
    NOR,
    AND,
    XOR,
    RSH,
    LDI,
    ADI,
    JMP,
    BRH,
    CAL,
    RET,
    LOD,
    STR,
    
    //Pseudo instructions
    CMP,
    MOV,
    LSH,
    INC,
    DEC,
    NOT,
    NEG
}
