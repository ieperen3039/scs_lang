use crate::{scs_lexer::{create_scs_lexer, ScsTokens}};

#[test]
fn lexer_small_execute() {
    let program = r#"
        function(parameter1)
            curry_fn1
            curry_fn2(parameter2)
            > Type1 parameter3

        parameter3
            curry_fn3
            > return
    "#;
    let lexer = create_scs_lexer();
    let tokens = {
        let tokens = lexer.read_all(program);

        assert!(tokens.is_ok(), "{:?}", tokens.unwrap_err());
        tokens.unwrap()
    };

    let expected = [
        ScsTokens::Whitespace,
        ScsTokens::Name, // function
        ScsTokens::ParenthesisOpen,
        ScsTokens::Name,
        ScsTokens::ParenthesisClose,
        ScsTokens::Whitespace,
        ScsTokens::Name, // curry_fn1
        ScsTokens::Whitespace,
        ScsTokens::Name, // curry_fn2
        ScsTokens::ParenthesisOpen,
        ScsTokens::Name, // parameter2
        ScsTokens::ParenthesisClose,
        ScsTokens::Whitespace,
        ScsTokens::AngleBracketClose,
        ScsTokens::Whitespace, // Type1
        ScsTokens::Name,
        ScsTokens::Whitespace,
        ScsTokens::Name,
        ScsTokens::Whitespace,
        ScsTokens::Name,
        ScsTokens::Whitespace,
        ScsTokens::Name,
        ScsTokens::Whitespace,
    ];

    for i in 0..expected.len() {
        assert!(tokens.len() > i);
        assert_eq!(tokens[i].class, expected[i], "\n index = {}, token = {}", i, tokens[i].slice);
    }
}