object StoreClass extends Enumeration {

 val SC_GLOBAL = Value(0x00f0)
 val SC_LOCAL = Value(0x00f1)
 val SC_LLOCAL = Value(0x00f2)
 val SC_CMP = Value(0x00f3)
 val SC_VALMAS = Value(0x00ff)
 val SC_LVAL = Value(0x0100)
 val SC_SYM = Value(0x0200)
 val SC_ANOM = Value(0x10000000)
 val SC_STRUCT = Value(0x20000000)
 val SC_MEMBER = Value(0x40000000)
 val SC_PARAMS = Value(0x80000000)

}