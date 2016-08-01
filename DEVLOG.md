

[2016.08.01] {Digging into the ARM bug a bit further.}

Here is the cmm from the "mkfoo" function from 7.6.3:

```
 mkfoo_rgE_entry()
         { label: mkfoo_rgE_info
           rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} }
         }
     cAO:
         Hp = Hp + 16;
         if (Hp > HpLim) goto cAS;
         I64[Hp - 8] = Foo_con_info;
         I64[Hp + 0] = I64[Sp + 0];
         R1 = Hp - 7;
         Sp = Sp + 8;
         jump (I64[I64[Sp + 0]]); // [R1]
     cAT:
         R1 = mkfoo_rgE_closure;
         jump stg_gc_fun; // [R1]
     cAS:
         HpAlloc = 16;
         goto cAT;
 }]
```

Here it is from 7.10.3:

```
[section "data" {
     mkfoo_rnV_closure:
         const mkfoo_rnV_info;
 },
 mkfoo_rnV_entry() //  []
  { info_tbl: [(c1cS,
                       label: mkfoo_rnV_info
                       rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} })]
           stack_info: arg_space: 0 updfr_space: Nothing
         }
     {offset
       c1cS:
           _B1::P64 = P64[Sp];
           goto c1cU;
       c1cU:
           Hp = Hp + 16;
           if (Hp > HpLim) goto c1cW; else goto c1cV;
       c1cW:
           HpAlloc = 16;
           goto c1cT;
       c1cT:
           R1 = mkfoo_rnV_closure;
           call (stg_gc_fun)(R1) args: 16, res: 0, upd: 8;
       c1cV:
           I64[Hp - 8] = Foo_con_info;
           P64[Hp] = _B1::P64;
           _c1cR::P64 = Hp - 7;
	      R1 = _c1cR::P64;
           Sp = Sp + 8;
           call (I64[P64[Sp]])(R1) args: 8, res: 0, upd: 8;
     }
 }]
```

Here is the version with -O2:

```
[section "data" {
     mkfoo_closure:
         const mkfoo_info;
 },
 mkfoo_entry() //  []
         { info_tbl: [(c40M,
                       label: mkfoo_info
                       rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} })]
           stack_info: arg_space: 0 updfr_space: Nothing
         }
     {offset
       c40M:
           Hp = Hp + 16;
           if (Hp > HpLim) goto c40Q; else goto c40P;
       c40Q:
           HpAlloc = 16;
           R1 = mkfoo_closure;
           call (stg_gc_fun)(R1) args: 16, res: 0, upd: 8;
       c40P:
           I64[Hp - 8] = Foo_con_info;
           P64[Hp] = P64[Sp];
           R1 = Hp - 7;
           Sp = Sp + 8;
           call (I64[P64[Sp]])(R1) args: 8, res: 0, upd: 8;
     }
 }]
```