{scs_program, [
    {version_declaration, [
        {version_dialect, "scs"},
        {version, [
            {integer_literal, "0"},
            {integer_literal, "0"},
            {integer_literal, "0"}
        ]}
    ]},
    {include_declaration, [
        {string_literal, ""filesystem""}
    ]},
    {parameter_list, [
        {parameter, [
            {type_name, [
                {identifier, "FileName"}
            ]},
            {identifier, "input_file"}
        ]}
    ]},
    {function_block, [
        {statement, [
            {identifier, "input_file"},
            {mutator_application, [
                {selector_both, ":"},
                {usage_application, "?"},
                {static_function_call, [
                    {identifier, "File"},
                    {function_call_tail, [
                        {identifier, "open"}
                    ]}
                ]}
            ]},
            {mutator_method, [
                {selector_positive, ">"},
                {usage_method, ">"},
                {function_call_tail, [
                    {identifier, "read_to_string"}
                ]}
            ]},
            {mutator_application, [
                {selector_negative, "?"},
                {usage_application, "?"},
                {static_function_call, [
                    {identifier, "Error"},
                    {function_call_tail, [
                        {identifier, "new"}
                    ]}
                ]}
            ]},
            {mutator_method, [
                {selector_both, ":"},
                {usage_method, ">"},
                {function_call_tail, [
                    {identifier, "split_on"},
                    {argument_list, [
                        {string_literal, ""\n""}
                    ]}
                ]}
            ]},
            {mutator_assign, [
                {selector_both, ":"},
                {usage_assign, "="},
                {identifier, "input_string"}
            ]}
        ]},
        {statement, [
            {identifier, "input_string"},
            {mutator_assign, [
                {selector_both, ":"},
                {usage_assign, "="}
            ]}
        ]}
    ]}
]}