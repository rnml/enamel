((packages  (core))
 (includes  (../unbound ../graph))
 (libraries (unbound graph_lib))
 (syntax    (comparelib sexplib bin_prot))
 (modules (std_internal
           scc
           syntax
           module_system
           initial_context
           base
           main))
 (target (exe main)))
