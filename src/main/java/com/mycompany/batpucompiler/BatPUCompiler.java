/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */

package com.mycompany.batpucompiler;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author tyler
 */

enum TokenType {
    TOK_EOF,
    TOK_INVALID,
    
    TOK_IDENT,
    TOK_NUMBER, // chars are also converted to numbers a=1, b=2, c=3...

    // keywords
    TOK_FUNC,
    TOK_VAR,
    TOK_ARR,
    TOK_WHILE,
    TOK_IF,
    TOK_RETURN,
    TOK_HALT,

    // operators
    TOK_ASSIGN, // =
    TOK_PLUS, // +
    TOK_MINUS, // -
    TOK_AND, // &
    TOK_OR, // |
    TOK_XOR, // ^
    TOK_RSHIFT, // >>
    

    // punctuation
    TOK_LPAREN, // (
    TOK_RPAREN, // )
    TOK_LBRACE, // {
    TOK_RBRACE, // }
    TOK_SEMI, // ;
    TOK_COMMA, // ,
    TOK_RBRACK, // ]
    TOK_LBRACK, // [
    TOK_STAR, // *
    
    
    // Output keywords
    TOK_VAROUT, // VarOut(ident)
    TOK_STRMEM, // StrMem(literal (addr), var (value))
    TOK_LODMEM // LodMem(expression (addr), output (var))    output <- Memmory[addr]
}

enum ParseType {
    // Top level
    PROGRAM,
    FUNCTION,
    BLOCK,
    
    // Statements
    VAR_DECL, // var a;
    ARR_DECL, // arr b[5];
    VAR_ASSIGN, // a = expr;
    ARR_ASSIGN, // b[0] = expr;
    IF, // if var block
    WHILE, // while var
    HALT, // halt;
    RETURN, // return var;
    // Output Satatements
    VAROUT, // VarOut(Ident);
    STRMEM,
    LODMEM,
    
    
    // Expressions
    BINARY, // a + 1, a - 1
    LITERAL, // nubmer literals
    IDENTIFIER, // a
    DEREF_IDENT // *ptr
}

class ParseNode {
    ParseType type;
    ArrayList<ParseNode> children;
    int value; // for number literals
    String name; // for identifiers
    
    public ParseNode(ParseType type) {
        this.type = type;
        children = new ArrayList<>();
    }
    
    public ParseNode(ParseType type, ArrayList<ParseNode> children) {
        this.type = type;
        this.children = children;
    }

    public ParseNode(ParseType type, ArrayList<ParseNode> children, int value, String name) {
        this.type = type;
        this.children = children;
        this.value = value;
        this.name = name;
    }
    
    public void add_child(ParseNode child) {
        children.add(child);
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        buildString(sb, 0);
        return sb.toString();
    }

    private void buildString(StringBuilder sb, int depth) {
        // indent
        for (int i = 0; i < depth; i++) {
            sb.append("  ");
        }

        // node info
        sb.append(type.name());

        if (name != null) {
            sb.append(" (").append(name).append(")");
        }

        if (type == ParseType.LITERAL) {
            sb.append(" = ").append(value);
        }
        
        if(type == ParseType.ARR_ASSIGN) {
            sb.append(" " + value);
        }
        
        sb.append("\n");

        // recurse
        for (ParseNode child : children) {
            child.buildString(sb, depth + 1);
        }
    }
}

class Token {
    TokenType type;
    int value; // for numbers
    String text; // for idents
    int line; // for debuging

    public Token(TokenType type, int value, String text) {
        this.type = type;
        this.value = value;
        this.text = text;
    }
    
    public Token() {}
    
    @Override
    public String toString() {
        if(type == TokenType.TOK_IDENT) {
            return "TOK_IDENT(" + text + ")";
        }
        if(type == TokenType.TOK_NUMBER) {
            return "TOK_NUMBER(" + value + ")";
        }
        
        return type.name();
    }
    
}

public class BatPUCompiler {
    
    /*public static final String program = 
            """
            func main {
                var a;
                a = 10;
                if a {
                    a = a + 1;
                }
                while a {
                    a = a - 1;
                }
                halt;
            }
            """;*/
    /*public static String program = """
                                   func main {
                                   	var a = 1;
                                   	var b = 2;
                                   	var c = a + b;
                                   	VarOut(c);
                                   }
                                   """;*/
    /*public static String program = // this is a basic fibonacci program
            """
            func main {
                var a = 1;
                var b = 1;
                var c;
                var n = 10;
            
                while n {
                    c = a + b;
                    a = b;
                    b = c;
                    n = n - 1;
                    VarOut(c);
                }
                halt;
            }
            """;*/
    /*
    POINTERS AND ARRAYS PLAN EXPLAINED:
    
    Arrays are just a bunch of variables side by side in memory.
    
    text is a pointer to 'h'
    
    we make a copy of the text called ptr (we could increment text instead of making a whole new variable, but we'd lose the initial value of the position of the array)
    */
    
    public static String program = // hello world using char arrays
            """
            func main {
                arr text[10];
                text[0] = 'h';
                text[1] = 'e';
                text[2] = 'l';
                text[3] = 'l';
                text[4] = 'o';
                text[5] = 'w';
                text[6] = 'o';
                text[7] = 'r';
                text[8] = 'l';
                text[9] = 'd';
                
                var ptr = text;
            
                StrMem(0, 249);
                
                var count = 10;
            
                while count {
                    StrMem(*ptr, 247);
                    ptr = ptr + 1;
                    count = count - 1;
                }
                
                StrMem(0, 248);
                
                halt; 
            }
            """;
    /*public static String program = // a much simpler hello world
            """
            func main {
                StrMem(0, 249);
                
                StrMem('h', 247);
                StrMem('e', 247);
                StrMem('l', 247);
                StrMem('l', 247);
                StrMem('o', 247);
                StrMem('w', 247);
                StrMem('o', 247);
                StrMem('r', 247);
                StrMem('l', 247);
                StrMem('d', 247);
            
                StrMem(0, 248);
                halt;
            }
            """;*/
    
    /*public static final String program = 
            """
            func main {
                var a;
                var b;
                a = 1;
                b = 50;
                c = add(a, b);
                VarOut(c);
                c = add(1, 10);
                VarOut(c);
                c = add(a + 1, 7);
                VarOut(c);
            }
            func add(a, b) {
                return a + b;
            }
            """;*/
    /*public static final String program =
            """
            func main {
                var or = 1;
                VarOut(or);
                halt;
            }
            """;*/
    
    static Token token;
    
    static int pidx = 0;
    
    static char currentChar = program.charAt(pidx);
    
    public static void next_token() {
        // Check for end of program
        if(pidx == program.length() - 1) {
            token.type = TokenType.TOK_EOF;
            return;
        }
        
        
        // skip whitespace
        while(Character.isWhitespace(currentChar)) {
            if(currentChar == '\n') { // update the current line being read
                token.line += 1;
            }
            incChar();
        }
        // identifiers and keywords
        if (isLetter(currentChar)) {
            String word = "";
            while(isLetter(currentChar)) {
                word += currentChar;
                incChar();
            }
            token.text = word;
            token.type = keyword(word);
            return;
        }
        
        // number litterals
        if(isNumb(currentChar)) {
            int val = 0;
            while(isNumb(currentChar)) {
                val = val * 10 + (currentChar - '0');
                incChar();
            }
            token.type = TokenType.TOK_NUMBER;
            token.value = val;
            return;
        }
        
        // convert char litterals to number litterals
        if(currentChar == '\'') {
            incChar();
            String alphabet = " abcdefghijklmnopqrstuvwxyz.!?";
            token.type = TokenType.TOK_NUMBER;
            token.value = alphabet.indexOf(currentChar);
            incChar();
            if(currentChar != '\'') {
                System.out.println("Expected '. instead found " + currentChar);
            }
            incChar();
            return;
        }
        
        // handle right shift operator (>>)
        if(currentChar == '>' && program.charAt(pidx + 1) == '>') {
            incChar();
            incChar();
            token.type = TokenType.TOK_RSHIFT;
        }
        
        // single character tokens
        switch(currentChar) {
            case '=' -> {
                token.type = TokenType.TOK_ASSIGN;
                token.text = "=";
                incChar();
                return;
            }
            case '+' -> {
                token.type = TokenType.TOK_PLUS;
                token.text = "+";
                incChar();
                return;
            }
            case '-' -> {
                token.type = TokenType.TOK_MINUS;
                token.text = "-";
                incChar();
                return;
            }
            case '&' -> {
                token.type = TokenType.TOK_AND;
                token.text = "&";
                incChar();
                return;
            }
            case '|' -> {
                token.type = TokenType.TOK_OR;
                token.text = "|";
                incChar();
                return;
            }
            case '^' -> {
                token.type = TokenType.TOK_XOR;
                token.text = "^";
                incChar();
                return;
            }
            
            case '(' -> {
                token.type = TokenType.TOK_LPAREN;
                token.text = "(";
                incChar();
                return;
            }
            case ')' -> {
                token.type = TokenType.TOK_RPAREN;
                token.text = ")";
                incChar();
                return;
            }
            case '{' -> {
                token.type = TokenType.TOK_LBRACE;
                token.text = "{";
                incChar();
                return;
            }
            case '}' -> {
                token.type = TokenType.TOK_RBRACE;
                token.text = "}";
                incChar();
                return;
            }
            case ';' -> {
                token.type = TokenType.TOK_SEMI;
                token.text = ";";
                incChar();
                return;
            }
            case ',' -> {
                token.type = TokenType.TOK_COMMA;
                token.text = ",";
                incChar();
                return;
            }
            case '[' -> {
                token.type = TokenType.TOK_LBRACK;
                token.text = "[";
                incChar();
                return;
            }
            case ']' -> {
                token.type = TokenType.TOK_RBRACK;
                token.text = "]";
                incChar();
                return;
            }
            case '*' -> {
                token.type = TokenType.TOK_STAR;
                token.text = "*";
                incChar();
                return;
            }
        }
        
        token.type = TokenType.TOK_INVALID;
        
    }
    
    static boolean isNumb(char c) {
        return (c >= '0' && c <= '9');
    }
    
    static boolean isLetter(char c) {
        return 
                ((currentChar >= 'A' && currentChar <= 'Z') ||
                (currentChar >= 'a' && currentChar <= 'z') ||
                currentChar == '_');
    }
    
    static void incChar() {
        pidx++;
        if(pidx > program.length() - 1) {
            pidx--;
        }
        currentChar = program.charAt(pidx);
    }
    
    static TokenType keyword(String word) {
        if(word.equals("func")) return TokenType.TOK_FUNC;
        if(word.equals("var")) return TokenType.TOK_VAR;
        if(word.equals("while")) return TokenType.TOK_WHILE;
        if(word.equals("if")) return TokenType.TOK_IF;
        if(word.equals("return")) return TokenType.TOK_RETURN;
        if(word.equals("halt")) return TokenType.TOK_HALT;
        if(word.equals("VarOut")) return TokenType.TOK_VAROUT;
        if(word.equals("StrMem")) return TokenType.TOK_STRMEM;
        if(word.equals("LodMem")) return TokenType.TOK_LODMEM;
        if(word.equals("arr")) return TokenType.TOK_ARR;
        return TokenType.TOK_IDENT;
    }
    
    // Expect function advances lexer to the next token and checks if it matches type
    static void expect(TokenType type) {
        next_token();
        if(token.type != type) {
            // throw a compiler fit
            System.out.println("Expected: " + type.name() + ". Instead found: " + token.type.name());
            Thread.dumpStack();
            System.exit(-1);
        }
        
    }
    
    // Parser
    static ParseNode parse_program() {
        ParseNode baseNode = new ParseNode(ParseType.PROGRAM);
        
        
        // parse all functions
        while(token.type != TokenType.TOK_EOF) {
            baseNode.add_child(parse_function());
        }
        
        return baseNode;
    }
    
    static ParseNode parse_function() {
        expect(TokenType.TOK_FUNC);
        
        expect(TokenType.TOK_IDENT);
        ParseNode func = new ParseNode(ParseType.FUNCTION);
        func.name = token.text;
        
        func.add_child(parse_block());
        return func;
    }
    
    static ParseNode parse_block() {
        expect(TokenType.TOK_LBRACE);
        
        ParseNode block = new ParseNode(ParseType.BLOCK);
        
        // consume {
        next_token();
        
        while (token.type != TokenType.TOK_RBRACE) {
            block.add_child(parse_statement());
        }
        next_token(); // consume }
        return block;
    }
    
    static ParseNode parse_statement() {
        if(token.type == TokenType.TOK_VAR) return parse_var_decl();
        if(token.type == TokenType.TOK_IF) return parse_if();
        if(token.type == TokenType.TOK_WHILE) return parse_while();
        if(token.type == TokenType.TOK_HALT) return parse_halt();
        if(token.type == TokenType.TOK_IDENT) return parse_assignment();
        if(token.type == TokenType.TOK_VAROUT) return parse_varout();
        if(token.type == TokenType.TOK_STRMEM) return parse_strmem();
        //if(token.type == TokenType.TOK_LODMEM) return parse_lodmem();
        if(token.type == TokenType.TOK_ARR) return parse_arr_decl();
        
        
        System.out.println("Unknown statement. Token: " + token.type.name());
        System.exit(-1);
        return null;
    }
    
    static ParseNode parse_arr_decl() {
        // arr text[10];
        expect(TokenType.TOK_IDENT);
        String ident = token.text;
        expect(TokenType.TOK_LBRACK);
        expect(TokenType.TOK_NUMBER);
        int len = token.value;
        expect(TokenType.TOK_RBRACK);
        expect(TokenType.TOK_SEMI);
        next_token();
        ParseNode node = new ParseNode(ParseType.ARR_DECL);
        node.value = len;
        node.name = ident;
        return node;
    }
    
    static ParseNode parse_strmem() {
        ParseNode strmem = new ParseNode(ParseType.STRMEM);
        expect(TokenType.TOK_LPAREN);
        next_token();
        strmem.children.add(parse_expression());
        if (token.type != TokenType.TOK_COMMA) {
            System.out.println("Expected comma, instead found " + token.type.name());
            System.exit(-1);
        }
        
        expect(TokenType.TOK_NUMBER);
        ParseNode addr = new ParseNode(ParseType.LITERAL);
        addr.value = token.value;
        strmem.children.add(addr);
        
        expect(TokenType.TOK_RPAREN);
        expect(TokenType.TOK_SEMI);
        next_token();
        return strmem;
    }
    
    static ParseNode parse_varout() {
        expect(TokenType.TOK_LPAREN);
        expect(TokenType.TOK_IDENT);
        
        ParseNode varOutNode = new ParseNode(ParseType.VAROUT);
        ParseNode ident = new ParseNode(ParseType.IDENTIFIER);
        ident.name = token.text;
        varOutNode.add_child(ident);
        expect(TokenType.TOK_RPAREN);
        expect(TokenType.TOK_SEMI);
        next_token(); // consume ;
        return varOutNode;
    }
    
    static ParseNode parse_var_decl() {
        expect(TokenType.TOK_IDENT);
        String varIdent = token.text;
        
        ParseNode decl = new ParseNode(ParseType.VAR_DECL);
        decl.name = varIdent;
        
        //expect(TokenType.TOK_SEMI);
        next_token();
        // check if variable is being assigned or just declared
        if(token.type == TokenType.TOK_ASSIGN) {
            ParseNode assign = new ParseNode(ParseType.VAR_ASSIGN);
            assign.name = varIdent;
            next_token(); // consume = 
            assign.add_child(parse_expression());
            decl.add_child(assign);
        } 
        
        // consume ;
        next_token();
        return decl;
    }
    
    static ParseNode parse_if() {
        expect(TokenType.TOK_IDENT);
        ParseNode compIdent = new ParseNode(ParseType.IDENTIFIER);
        compIdent.name = token.text;
        
        ParseNode nodeIf = new ParseNode(ParseType.IF);
        
        nodeIf.add_child(compIdent);
        nodeIf.add_child(parse_block());
        
        return nodeIf;
    }
    
    static ParseNode parse_while() {
        expect(TokenType.TOK_IDENT);
        ParseNode compIdent = new ParseNode(ParseType.IDENTIFIER);
        compIdent.name = token.text;
        
        ParseNode nodeWhile = new ParseNode(ParseType.WHILE);
        
        nodeWhile.add_child(compIdent);
        
        nodeWhile.add_child(parse_block());
        return nodeWhile;
    }
    
    static ParseNode parse_halt() {
        next_token(); // consume halt
        next_token(); // consume ;
        return new ParseNode(ParseType.HALT);
    }
    
    static ParseNode parse_assignment() {
        String name = token.text;
        
        // check if this is an array
        next_token();
        if(TokenType.TOK_LBRACK == token.type) {
            ParseNode assign = new ParseNode(ParseType.ARR_ASSIGN);
            expect(TokenType.TOK_NUMBER);
            assign.name = name; // base
            assign.value = token.value; // idx to modify
            
            expect(TokenType.TOK_RBRACK);
            expect(TokenType.TOK_ASSIGN);
            
            next_token();
            
            // parse the expression
            assign.add_child(parse_expression());
            
            //check that token is semi without advancing lexer
            if(token.type != TokenType.TOK_SEMI) {
                System.out.println("Expected TOK_SEMI. Found: " + token.type.name());
                System.exit(-1);
            }
            // consume ;
            next_token();
            
            return assign;
        } else if(token.type != TokenType.TOK_ASSIGN) {
            System.out.println("Expected TOK_ASSIGN. Found: " + token.type.name());
            System.exit(-1);
        }
        
        ParseNode assign = new ParseNode(ParseType.VAR_ASSIGN);
        assign.name = name;
        
        next_token(); // consume =
        assign.add_child(parse_expression());
        
        //expect(TokenType.TOK_SEMI);
        //check that token is semi without advancing lexer
        if(token.type != TokenType.TOK_SEMI) {
            System.out.println("Expected TOK_SEMI. Found: " + token.type.name());
            System.exit(-1);
        }
        // consume ;
        next_token();
        return assign;
        
    }
    
    static ParseNode parse_expression() {
        return parse_additive();
    }
    
    static ParseNode parse_additive() {
        ParseNode left = parse_primary();
        
        while(token.type == TokenType.TOK_PLUS 
                || token.type == TokenType.TOK_MINUS 
                || token.type == TokenType.TOK_AND 
                || token.type == TokenType.TOK_OR 
                || token.type == TokenType.TOK_XOR
                || token.type == TokenType.TOK_RSHIFT) {
            String op = token.text; // operator
            next_token();
            
            ParseNode right = parse_primary();
            
            ParseNode bin = new ParseNode(ParseType.BINARY);
            bin.name = op; // store operator here
            bin.add_child(left);
            bin.add_child(right);
            
            left = bin;
            
        }
        
        return left;
        
    }
    
    static ParseNode parse_primary() {
        // handle pointers
        if(token.type == TokenType.TOK_STAR) {
            expect(TokenType.TOK_IDENT);
            
            ParseNode n = new ParseNode(ParseType.DEREF_IDENT);
            n.name = token.text;
            next_token();
            return n;
        }
        
        if(token.type == TokenType.TOK_NUMBER) {
            ParseNode n = new ParseNode(ParseType.LITERAL);
            n.value = token.value;
            next_token();
            return n;
        }
        
        if(token.type == TokenType.TOK_IDENT) {
            ParseNode n = new ParseNode(ParseType.IDENTIFIER);
            n.name = token.text;
            next_token();
            return n;
        }
        
        if(token.type == TokenType.TOK_LPAREN) {
            next_token();
            ParseNode e = parse_expression();
            expect(TokenType.TOK_RPAREN);
            return e;
        }
        
        System.out.println("Expected expression: instead got:" + token.type.name());
        return null;
        
    }
    
    // code gen
    static String[] MemAllocation;
    static ArrayList<ASMInst> instructions;
    
    static int currentPC() {
        return instructions.size();
    }
    
    static ASMInst emitBranchPlaceholder(int cond) {
        ASMInst br = ASMInst.branch(cond, 0); // addr set later
        emit(br);
        return br;
    }
    
    static void compile(ParseNode node) {
        switch (node.type) {
            case PROGRAM -> compileProgram(node);
            case FUNCTION -> compileFunction(node);
            case BLOCK -> compileBlock(node);

            case VAR_DECL -> compileVarDecl(node);
            case VAR_ASSIGN -> compileVarAssign(node);
            case ARR_ASSIGN -> compileArrAssign(node);
            case ARR_DECL -> compileArrDecl(node);
            case IF -> compileIf(node);
            case WHILE -> compileWhile(node);
            case HALT -> emit(new ASMInst(InstType.HLT));
            case VAROUT -> compileVarOut(node);
            case STRMEM -> compileStrMem(node);
            //case RETURN -> compileReturn(node); TODO: impliment functions correctly in parse tree and in ASM

            default -> throw new RuntimeException("Invalid statement node: " + node.type);
        }
    }
    
    static void compileArrAssign(ParseNode node) {
        /*
        ASM:
        (some expression -> rTemp1)
        LDI r4 (array base)
        LDI rTemp2 (idx + 1)
        ADD r4 rTemp2 r4
        STR r4 rTemp1
        */
        int temp1 = compileExpr(node.children.get(0));
        emit(ASMInst.regImm(InstType.LDI, 4, getVar(node.name)));
        int temp2 = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, temp2, node.value + 1));
        emit(ASMInst.reg3(InstType.ADD, 4, temp2, 4));
        emit(ASMInst.reg3(InstType.STR, 4, temp1, 0));
        
        freeTemp(temp1);
        freeTemp(temp2);
        
    }
    
    static void compileArrDecl(ParseNode node) {
        allocateArr(node.name, node.value);
        // point the array ident to the first value of the array
        /*
        ASM:
        LDI r4 (arrayIdent)
        LDI rTemp (0 arrayIdent)
        STR r4 rTemp
        */
        emit(ASMInst.regImm(InstType.LDI, 4, getVar(node.name)));
        int temp = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, temp, getVar("0 " + node.name)));
        emit(ASMInst.reg3(InstType.STR, 4, temp, 0));
        freeTemp(temp);
    }
    
    static void compileVarOut(ParseNode node) {
        /*
        ASM:
        LDI r4 varPtr
        LOD r4 r4
        LDI rTemp 250
        STR rTemp r4
        */
        
        emit(ASMInst.regImm(InstType.LDI, 4, getVar(node.children.get(0).name)));
        emit(ASMInst.reg3(InstType.LOD, 4, 4, 0));
        int rTemp = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, rTemp, 250));
        emit(ASMInst.reg3(InstType.STR, rTemp, 4, 0));
        freeTemp(rTemp);
    }
    
    static void compileStrMem(ParseNode node) {
        /*
        ASM:
        expression -> tempReg
        LDI r4 addr
        STR r4 tempReg
        */
        int resultReg = compileExpr(node.children.get(0));
        
        emit(ASMInst.regImm(InstType.LDI, 4, node.children.get(1).value));
        emit(ASMInst.reg3(InstType.STR, 4, resultReg, 0));
        freeTemp(resultReg);
    }
    
    static void compileProgram(ParseNode node) {
        for(ParseNode n : node.children) {
            compile(n);
        }
    }
    
    static void compileFunction(ParseNode node) {
        compile(node.children.get(0)); // compile the block
    }
    
    static void compileBlock(ParseNode node) {
        for(ParseNode statement : node.children) {
            compile(statement);
        }
    }
    
    static void compileIf(ParseNode node) {
        /*
        ASM Pattern:
        LDI r4, addr(condition)
        LOD r1, r4
        SUB r1, r0, r0  (set flags based on condition)
        BRH EQ if_end  (if condition == 0 skip body)
        
        (body code here)
        
        if_end:
        */
        
        int addr = getVar(node.children.get(0).name);
        
        int condReg = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, 4, addr));
        emit(ASMInst.reg3(InstType.LOD, condReg, 4, 0));
        
        // CMP CondReg 0
        emit(ASMInst.reg3(InstType.SUB, condReg, 0, 0));
        
        // Branch if 0
        ASMInst br = emitBranchPlaceholder(00/*EQ*/);
        
        freeTemp(condReg);
        
        // compile block
        compile(node.children.get(1));
        
        
        // patch branch target
        br.addr = currentPC();
        
    }
    
    static void compileWhile(ParseNode node) {
        /*
        ASM Pattern:
        
loop:   LDI r4, addr(condition)
        LOD r4, r1
        SUB r1, r0, r0  (set flags based on condition) PSEUDO: CMD r1 r0 r0
        BRH EQ loop_end  (if condition == 0 skip body)
        
        (body code here)
        
        JMP loop
loop_end:
        */
        int loopStart = currentPC();
        int addr = getVar(node.children.get(0).name);
        
        int condReg = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, 4, addr));
        emit(ASMInst.reg3(InstType.LOD, 4, 1, 0));
        
        emit(ASMInst.reg3(InstType.SUB, condReg, 0, 0));
        
        ASMInst br = emitBranchPlaceholder(00/*EQ*/);
        
        freeTemp(condReg);
        
        compile(node.children.get(1));
        
        // jump to start
        emit(ASMInst.jump(loopStart));
        
        // exit
        br.addr = currentPC();
    }
    
    static void compileVarDecl(ParseNode node) {
        allocateMem(node.name);
        if(node.children.size() == 1) {
            compileVarAssign(node.children.get(0));
        }
    }
    
    static void compileVarAssign(ParseNode n) {
        // LDI to a temp address and then STR to the address
        int value = compileExpr(n.children.get(0));
        
        int addr = getVar(n.name);
        
        emit(ASMInst.regImm(InstType.LDI, 4, addr)); // LDI r4 pointer
        emit(ASMInst.reg3(InstType.STR, 4, value, 0)); // STR r4 value (Takes the value in r4 and saves it to memory address value)
        
        freeTemp(value);
        
        /*
        ex.
        Name -- addr
        var1    10
        
        initial regester 4
        r4 = 0;
        
        execute LDI
        LDI r4 10
        
        then use the value of r4 to point to the variable in memory
        
        execute STR
        STR r4 (result from compileExpr)
        
        So in total var assigns compile to:
        
        (Some compileExpr) -> tempReg
        LDI r4 (constant pointer to var)
        STR r4 tempReg
        */
        
    }
    
    static int compileExpr(ParseNode n) {
        return switch(n.type) {
            case LITERAL -> compileLiteral(n);
            case IDENTIFIER -> compileIdentifier(n);
            case DEREF_IDENT -> compileDerfIdent(n);
            case BINARY -> compileBinary(n);
            
            default -> {
                throw new RuntimeException("Not an expression:" + n.type);
            }
        };
    }
    
    static int compileDerfIdent(ParseNode n) {
        // load pointer value from variable
        ParseNode identParse = new ParseNode(ParseType.IDENTIFIER);
        identParse.name = n.name;
        int ptrReg = compileIdentifier(identParse);
        
        // load value from memory at that address
        int valueReg = allocTemp();
        emit(ASMInst.reg3(InstType.LOD, ptrReg, valueReg, 0));
        freeTemp(ptrReg);
        
        return valueReg;
    }
    
    static int compileIdentifier(ParseNode n) {
        int r = allocTemp();
        int addr = getVar(n.name);
        
        
        emit(ASMInst.regImm(InstType.LDI, 4, addr)); // r4 <- address
        emit(ASMInst.reg3(InstType.LOD, 4, r, 0)); // r = memory[r4]
        
        return r;
    }
    
    static int compileLiteral(ParseNode n) {
        int r = allocTemp();
        emit(ASMInst.regImm(InstType.LDI, r, n.value)); // load the literal to a temp reg
        return r;
    }
    
    static int compileBinary(ParseNode n) {
        // evaluate operands
        int left = compileExpr(n.children.get(0));
        int right = compileExpr(n.children.get(1));
        
        int dst = allocTemp();
        
        /*
        TOK_PLUS, // +
        TOK_MINUS, // -
        TOK_AND, // &
        TOK_OR, // |
        TOK_XOR, // ^
        TOK_RSHIFT, // >>
        */
        
        InstType op = switch(n.name) {
            case "+" -> InstType.ADD;
            case "-" -> InstType.SUB;
            case "&" -> InstType.AND;
            case "^" -> InstType.XOR; //TODO: IMPLIMENT | AND >>
            case "|" -> null; // to be implimented using AND + XOR
            case ">>" -> InstType.RSH;
            default -> throw new RuntimeException("Unknown operator");
        };
        
        if(op == null) {
            // (left XOR right) XOR (left AND right)
            emit(ASMInst.reg3(InstType.XOR, left, right, dst));
            int temp = allocTemp();
            emit(ASMInst.reg3(InstType.AND, left, right, temp));
            emit(ASMInst.reg3(InstType.XOR, dst, temp, dst));
            
            freeTemp(left);
            freeTemp(right);
            freeTemp(temp);
            return dst;
        }
        
        emit(ASMInst.reg3(op, left, right, dst));
        
        freeTemp(left);
        freeTemp(right);
        
        return dst;
    }
    
    static int[] temps = {1, 2, 3};
    static boolean[] used;

    static int allocTemp() {
        for (int r : temps)
            if (!used[r]) {
                used[r] = true;
                return r;
            }
        throw new RuntimeException("Out of temp registers");
    }

    static void freeTemp(int r) {
        used[r] = false;
    }

    
    static int allocateArr(String name, int len) {
        int ptr = allocateMem(name); // this is funny. its kinda like a ptr to a ptr
        for(int i = 0; i < len; i++) {
            allocateMem(i + " text"); // this has to be the hackiest solution ever. Idents can't start with a number or contain a space
        }
        return ptr;
    }
    
    static int allocateMem(String name) {
        for(int i = 0; i < MemAllocation.length; i++) {
            if(MemAllocation[i] == null) {
                MemAllocation[i] = name;
                System.out.println("Mapped var " + name + " to addr: " + i);
                return i;
            }
        }
        System.out.println("Failed to allocate: " + name);
        System.exit(-1);
        return -1;
    }
    
    static int getVar(String name) {
        for(int i = 0; i < MemAllocation.length; i++) {
            // avoid null
            if(MemAllocation[i] == null) {
                continue;
            }
            
            if(MemAllocation[i].equals(name)) {
                return i;
            }
        }
        System.out.println("Failed to find variable: " + name);
        Thread.dumpStack();
        System.exit(-1);
        return -1;
    }
    
    //static int r4knownValue;
    
    static void emit(ASMInst inst) {
        /*// optimize use of r4 TODO: do this after instructions are emited because JMP and BRH rely on PC addresses
        if((inst.type == InstType.LOD && inst.rb == 4) || 
                (inst.type == InstType.ADD && inst.rd == 4) ||
                (inst.type == InstType.SUB && inst.rd == 4) ||
                (inst.type == InstType.AND && inst.rd == 4) ||
                (inst.type == InstType.XOR && inst.rd == 4) ||
                (inst.type == InstType.RSH && inst.rd == 4) ||
                (inst.type == InstType.NOR && inst.rd == 4) ||
                
                (inst.type == InstType.JMP) ||
                (inst.type == InstType.BRH)) {
            r4knownValue = -1000;
        }
        
        if(inst.type == InstType.LDI && inst.rd == 4) {
            if(r4knownValue == inst.imm) {
                return; // omit instruction
            } else {
                r4knownValue = inst.imm;
            }
        }*/
        
        inst.pc = instructions.size();
        instructions.add(inst);
    }
    
    public static void main(String[] args) {
        token = new Token();
        /*while(token.type != TokenType.TOK_EOF) {
            
            next_token();
            System.out.println(token.toString());
        }*/
        
        ParseNode parsed = parse_program();
        
        used = new boolean[16];
        MemAllocation = new String[240]; // our system has 256 bytes of memory (note addresses 240-255 are reserved for I/O)
        instructions = new ArrayList<>();
        
        System.out.print(parsed);
        compile(parsed);
        
        for (ASMInst i : instructions) {
            System.out.println(i);
        }
        
    }
    
    public static void loadProgramFile(String filePath) throws IOException {
        byte[] bytes = Files.readAllBytes(Paths.get(filePath));
        program = new String(bytes, StandardCharsets.UTF_8);
    }
    
}
