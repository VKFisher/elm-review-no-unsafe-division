module NoUnsafeDivision exposing (rule)

{-| This module contains rules that have to do with forbiddinmg unsafe division operations


# Rules

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Maybe.Extra as ME
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids unsafe usages of `(/)`, `(//)`, `modBy` and `remainderBy`
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnsafeDivision" Nothing
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type UnsafeOperation
    = Divide
    | IntegerDivide
    | ModBy
    | RemainderBy


expressionVisitor : Node Expression -> Maybe (Node Expression) -> ( List (Error {}), Maybe (Node Expression) )
expressionVisitor node context =
    selectUnsafeOperation node context
        |> Maybe.map (\( op, errNode ) -> unsafeOperationToError op errNode)
        |> ME.toList
        |> (\errList -> ( errList, Just node ))


selectUnsafeOperation : Node Expression -> Maybe (Node Expression) -> Maybe ( UnsafeOperation, Node Expression )
selectUnsafeOperation node context =
    let
        unsafeOperationFromString : String -> Maybe UnsafeOperation
        unsafeOperationFromString s =
            case s of
                "/" ->
                    Just Divide

                "//" ->
                    Just IntegerDivide

                "modBy" ->
                    Just ModBy

                "remainderBy" ->
                    Just RemainderBy

                _ ->
                    Nothing

        -- allow native division functions when the divisor is a non-zero literal value
        isNonZeroLiteral : Node Expression -> Bool
        isNonZeroLiteral n =
            case Node.value n of
                Expression.Integer x ->
                    x /= 0

                Expression.Floatable x ->
                    x /= 0

                _ ->
                    False

        excludeNonZeroDivisors : Node Expression -> Maybe a -> Maybe a
        excludeNonZeroDivisors divisorNode =
            ME.filter (always <| not <| isNonZeroLiteral divisorNode)
    in
    case Node.value node of
        Expression.OperatorApplication op _ _ right ->
            unsafeOperationFromString op
                |> excludeNonZeroDivisors right
                |> Maybe.map (\x -> ( x, node ))

        Expression.PrefixOperator childOp ->
            if List.member childOp [ "/", "//" ] then
                case context of
                    Just parentNode ->
                        case Node.value parentNode of
                            Expression.Application [ opNode, _, right ] ->
                                case Node.value opNode of
                                    Expression.PrefixOperator op ->
                                        unsafeOperationFromString op
                                            |> excludeNonZeroDivisors right
                                            |> Maybe.map (\x -> ( x, parentNode ))

                                    _ ->
                                        Nothing

                            _ ->
                                unsafeOperationFromString childOp
                                    |> Maybe.map (\x -> ( x, node ))

                    _ ->
                        unsafeOperationFromString childOp
                            |> Maybe.map (\x -> ( x, node ))

            else
                Nothing

        Expression.FunctionOrValue _ childFn ->
            if List.member childFn [ "modBy", "remainderBy" ] then
                case context of
                    Just parentNode ->
                        case Node.value parentNode of
                            Expression.Application (fnNode :: first :: _) ->
                                case Node.value fnNode of
                                    Expression.FunctionOrValue _ fn ->
                                        unsafeOperationFromString fn
                                            |> excludeNonZeroDivisors first
                                            |> Maybe.map (\x -> ( x, parentNode ))

                                    _ ->
                                        Nothing

                            _ ->
                                unsafeOperationFromString childFn
                                    |> Maybe.map (\x -> ( x, node ))

                    _ ->
                        unsafeOperationFromString childFn
                            |> Maybe.map (\x -> ( x, node ))

            else
                Nothing

        _ ->
            Nothing


unsafeOperationToError : UnsafeOperation -> Node Expression -> Error {}
unsafeOperationToError unsafeOperation node =
    let
        data : { message : String, details : List String }
        data =
            case unsafeOperation of
                Divide ->
                    { message = "Use `Basics.Extra.safeDivide` instead of the native `/`"
                    , details =
                        [ "Using the native division operator can result in values like `NaN` or `Infinity`, which may lead to unwanted behavior."
                        ]
                    }

                IntegerDivide ->
                    { message = "Use `Basics.Extra.safeIntegerDivide` instead of the native `//`"
                    , details =
                        [ "`x // 0` produces 0, which is a somewhat arbitrary result and may lead to unwanted behavior in your code."
                        , "In cases where you do want the result to be 0, it is better to define it explicitly through `Maybe.withDefault 0 <| safeIntegerDivide x y`."
                        ]
                    }

                ModBy ->
                    { message = "Use `Basics.Extra.safeModBy` instead of the native `modBy`"
                    , details =
                        [ "`modBy 0 x` is one of the very few ways to cause a runtime exception in Elm."
                        ]
                    }

                RemainderBy ->
                    { message = "Use `Basics.Extra.safeRemainderBy` instead of the native `remaiderBy`"
                    , details =
                        [ "`remainderBy 0 x` produces `NaN`, which is probably not what you want."
                        ]
                    }

        range : Range
        range =
            Node.range node
    in
    Rule.error data range
