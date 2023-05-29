use crate::{scs_lexer::ScsToken, lexer::Lexer};

#[test]
fn lexer_small_execute() {
    let program = r#"
        function(parameter1)
            curry_fn1
            curry_fn2(parameter2)
            > Type1 parameter3
    "#;
    let lexer = Lexer::new_scs();
    let tokens = {
        let tokens = lexer.read_all(program);

        assert!(tokens.is_ok(), "{:?}", tokens.unwrap_err());
        tokens.unwrap()
    };

    let expected = [
        ScsToken::Whitespace,
        ScsToken::Name, // function
        ScsToken::ParenthesisOpen,
        ScsToken::Name,
        ScsToken::ParenthesisClose,
        ScsToken::Whitespace,
        ScsToken::Name, // curry_fn1
        ScsToken::Whitespace,
        ScsToken::Name, // curry_fn2
        ScsToken::ParenthesisOpen,
        ScsToken::Name, // parameter2
        ScsToken::ParenthesisClose,
        ScsToken::Whitespace,
        ScsToken::AngleBracketClose,
        ScsToken::Whitespace, 
        ScsToken::Name, // Type1
        ScsToken::Whitespace,
        ScsToken::Name,
        ScsToken::Whitespace,
    ];

    for i in 0..expected.len() {
        assert!(tokens.len() > i);
        assert_eq!(tokens[i].class, expected[i], "\n index = {}, token = {}", i, tokens[i].slice);
    }
}