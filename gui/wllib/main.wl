!@import bc;

!:global menu_categories = $[
    "literals",
    "variables",
    "arithmetics",
    "functions",
    "routing",
    "nodes",
];

!:global wichtext_click = {!(cmd, line, frag) = @;
    std:displayln "CLICK" @;
};

!:global block_fun_change = {
    std:displayln "BLOCKFUN CHANGE:" @;

    !fun = bc:fun[];
    !ast = bc:generate_ast fun;
    !dump = ast.dump[];

    !wt_dump_lines = $[];
    iter l ($p('\n', 0) dump) {
        std:displayln "LINE:" l;
        std:push wt_dump_lines ~ $F "[f14:]{}" l;
    };

    bc:wichtext_set 0 ~
        std:str:cat
            "[f40:WLambda API!]\n\n"
            (std:str:join "\n" wt_dump_lines);
};

!:global init = {
    std:displayln "INIT";

    bc:wichtext_set 0 ~
        std:str:cat
            "[f40:WLambda API!]\n\n"
            "And click [af18:Click1]\n"
            "And here: [af18:Click2]";

    !lang = bc:lang[];
    lang.define ${
        category    = "nodes",
        name        = "foo",
        description = "Something foo",
        rows        = 3,
        area_count  = 3,
        color       = 8,
        user_input  = :integer,
        inputs      = $["XXX", $n, "OOO"],
        outputs     = $[],
    };
};
