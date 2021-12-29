module NoUnsafeDivision exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnsafeDivision" Nothing
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> Maybe (Node Expression) -> ( List (Error {}), Maybe (Node Expression) )
expressionVisitor node context =
    let
        noDivisionOperators : String -> Node Expression -> ( List (Error {}), Maybe (Node Expression) )
        noDivisionOperators op errorNode =
            case op of
                "/" ->
                    ( [ error errorNode "Use `Basics.Extra.safeDivide` instead of the native `/`" ], Just node )

                "//" ->
                    ( [ error errorNode "Use `Basics.Extra.safeIntegerDivide` instead of the native `//`" ], Just node )

                _ ->
                    ( [], Just errorNode )

        noPrefixDivisionOperators : String -> Node Expression -> ( List (Error {}), Maybe (Node Expression) )
        noPrefixDivisionOperators op errorNode =
            case op of
                "/" ->
                    ( [ error errorNode "Use `Basics.Extra.safeDivide` instead of the native `(/)`" ], Just node )

                "//" ->
                    ( [ error errorNode "Use `Basics.Extra.safeIntegerDivide` instead of the native `(//)`" ], Just node )

                _ ->
                    ( [], Just node )

        noDivisionFunctions : String -> Node Expression -> ( List (Error {}), Maybe (Node Expression) )
        noDivisionFunctions fnName errorNode =
            case fnName of
                "modBy" ->
                    ( [ error errorNode "Use `Basics.Extra.safeModBy` instead of the native `modBy`" ], Just node )

                "remainderBy" ->
                    ( [ error errorNode "Use `Basics.Extra.safeRemainderBy` instead of the native `remainderBy`" ], Just node )

                _ ->
                    ( [], Just node )
    in
    case Node.value node of
        Expression.OperatorApplication op _ _ right ->
            case Node.value right of
                Expression.Integer x ->
                    if x == 0 then
                        noDivisionOperators op node

                    else
                        ( [], Just node )

                Expression.Floatable x ->
                    if x < 0.0000001 then
                        noDivisionOperators op node

                    else
                        ( [], Just node )

                _ ->
                    noDivisionOperators op node

        Expression.PrefixOperator childOp ->
            case context of
                Just parentNode ->
                    case Node.value parentNode of
                        Expression.Application [ opNode, _, right ] ->
                            case Node.value opNode of
                                Expression.PrefixOperator op ->
                                    case Node.value right of
                                        Expression.Integer x ->
                                            if x == 0 then
                                                noPrefixDivisionOperators op parentNode

                                            else
                                                ( [], Just node )

                                        Expression.Floatable x ->
                                            if x < 0.0000001 then
                                                noPrefixDivisionOperators op parentNode

                                            else
                                                ( [], Just node )

                                        _ ->
                                            noPrefixDivisionOperators op parentNode

                                _ ->
                                    ( [], Just node )

                        _ ->
                            noPrefixDivisionOperators childOp node

                _ ->
                    noPrefixDivisionOperators childOp node

        Expression.FunctionOrValue _ childFn ->
            case context of
                Just parentNode ->
                    case Node.value parentNode of
                        Expression.Application (fnNode :: first :: _) ->
                            case Node.value fnNode of
                                Expression.FunctionOrValue _ fn ->
                                    case Node.value first of
                                        Expression.Integer x ->
                                            if x == 0 then
                                                noDivisionFunctions fn parentNode

                                            else
                                                ( [], Just node )

                                        Expression.Floatable x ->
                                            if x < 0.0000001 then
                                                noDivisionFunctions fn parentNode

                                            else
                                                ( [], Just node )

                                        _ ->
                                            noDivisionFunctions fn parentNode

                                _ ->
                                    ( [], Just node )

                        _ ->
                            noDivisionFunctions childFn node

                _ ->
                    noDivisionFunctions childFn node

        _ ->
            ( [], Just node )


error : Node Expression -> String -> Error {}
error node message =
    Rule.error
        { message = message
        , details =
            [ "Using unsafe division is one of the very few ways to cause a runtime exception in Elm. "
                ++ "Removing such functions increases our confidence that the compiled program is correct."
            ]
        }
        (Node.range node)
