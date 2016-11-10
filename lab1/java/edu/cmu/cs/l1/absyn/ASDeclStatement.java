//Mike Maxim
//Absyn for a variable declaration

package edu.cmu.cs.l1.absyn;

import edu.cmu.cs.l1.symbol.*;
public class ASDeclStatement extends ASStatement {

    /** Construct a declaration from a variable
     * @param p Position in source
     * @param vname Variable name
     */
    public ASDeclStatement(int p, Symbol vname)
	{
		super(p);
		v_name = vname;
		m_classname = "ASDeclStatement";
    }

    /** Return the symbol for the variable */
    public Symbol getVar() {
		return v_name;
    }

    private Symbol v_name;
}
