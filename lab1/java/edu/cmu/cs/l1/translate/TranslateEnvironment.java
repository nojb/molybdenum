// Mike Maxim
// Environment class

package edu.cmu.cs.l1.translate;

import edu.cmu.cs.l1.symbol.*;
import edu.cmu.cs.l1.tree.*;
import java.util.*;

public class TranslateEnvironment {

    public TranslateEnvironment() {
	m_env = new SymbolTable();
	m_init_vars = new Hashtable();
    }

    public void addEntry(Symbol name, IRExpression var) throws SymbolTableException {
	m_env.put(name,var);
    }

    public IRExpression getEntry(Symbol name) {
	return (IRExpression)m_env.get(name);
    }

    public void removeEntry(Symbol name) {
	m_env.remove(name);
    }

    public String toString() {
	String str="";
	str += m_env.toString() + "\n";
	return str;
    }

    public void markInitialized(Symbol name)
	{
		m_init_vars.put(name, new Object());
	}

	public boolean isInitialized(Symbol name)
	{
		Object o = m_init_vars.get(name);
		return (o != null);
	}

    private SymbolTable m_env;
	private Hashtable m_init_vars;
}
