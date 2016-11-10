//Mike Maxim
//Main class for the full compilation

package edu.cmu.cs.l1.main;

import edu.cmu.cs.l1.general.*;
import edu.cmu.cs.l1.parse.*;
import edu.cmu.cs.l1.absyn.*;
import edu.cmu.cs.l1.translate.*;
import edu.cmu.cs.l1.tree.*;
import edu.cmu.cs.l1.codegen.*;
import edu.cmu.cs.l1.x86.*;
import edu.cmu.cs.l1.temp.*;
import edu.cmu.cs.l1.assem.*;
import java.io.*;
import java.util.*;

public class Compiler {
    public Compiler() {
    }

    public static final int ABSYN=1, IR=2, ASSEM=3, COMPILE=4;

    public static void go(String filename, String outfile, int mode) throws Exception {
        int i, j;

        try {

            Globals.verbosePrintln("--- Parsing");

            Parse p = new Parse();
            p.parse(filename);

            if (mode == ABSYN) {
                ASPrintAbsyn printer = new ASPrintAbsyn(System.out);
                p.getProgram().visit(printer);
                System.out.println();
                Globals.shutdown(0);
            }

            Globals.verbosePrintln("--- Translating Abstract Syntax");

            Translator tr = new Translator();
            tr.translate(p);

            if (mode == IR) {
                IRPrint printer = new IRPrint(System.out);
                tr.getProgram().visit(printer);
                System.out.println();
                Globals.shutdown(0);
            }

            Globals.verbosePrintln("--- Generating Code");

            CodeGenerator cg = new X86CodeGenerator();
            TempMap regalloc;
            List asmcode = cg.codegen(tr.getProgram());

            Globals.verbosePrintln("--- Register Allocation");

            //The place where register allocation should take place
            StringBuffer code = new StringBuffer();
            regalloc = new DefaultMap();
            for (i = 0; i < asmcode.size(); i++) {
                Instruction ins = (Instruction) asmcode.get(i);
                code.append(ins.format(regalloc));
            }

            if (mode == IR) {
                System.out.println(code);
                System.out.println();
                Globals.shutdown(0);
            }

            File ofile = new File(outfile); ofile.createNewFile();
            PrintStream targetfile = new PrintStream(new FileOutputStream(ofile));
            targetfile.print(code);

            Globals.verbosePrintln("--- Done.");

        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Compilation aborted due to errors.");
            System.exit(-1);
        }

    }
}
