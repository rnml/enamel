((packages  (core))
 (includes  (../unbound))
 (libraries (unbound))
 (syntax    (comparelib sexplib bin_prot))
 (modules (std_internal
           scc
           f
           target
           module_system
           initial_context
           base
           main))
 (target (exe main)))
