break main
run

break uim_scm_eval_c_string
commands
  cont
end

break uim_scm_eval
commands
  cont
end

set height 0
cont
