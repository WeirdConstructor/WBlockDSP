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

!:global init = {
    std:displayln "INIT";

    bc:wichtext_set 0 "[f40:Foo Bar!]\n\nAnd click [af14:Here!]";

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
