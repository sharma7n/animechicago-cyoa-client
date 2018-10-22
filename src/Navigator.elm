module Navigator exposing (Navigator, back, canGoBack, canGoForward, forward, new, push, getCurrent)

import NonEmptyList as NE


type BackStack a
    = BackStack (NE.NonEmptyList a)


type ForwardStack a
    = ForwardStack (NE.NonEmptyList a)


type Navigator a
    = InitialNavigator a
    | ReturnedNavigator a (ForwardStack a)
    | MiddleNavigator a (BackStack a) (ForwardStack a)
    | TerminalNavigator a (BackStack a)


new : a -> Navigator a
new object =
    InitialNavigator object


canGoBack : Navigator a -> Bool
canGoBack navi =
    case navi of
        MiddleNavigator _ _ _ ->
            True

        TerminalNavigator _ _ ->
            True

        _ ->
            False


canGoForward : Navigator a -> Bool
canGoForward navi =
    case navi of
        ReturnedNavigator _ _ ->
            True

        MiddleNavigator _ _ _ ->
            True

        _ ->
            False


push : a -> Navigator a -> Navigator a
push next navi =
    case navi of
        InitialNavigator current ->
            TerminalNavigator next (BackStack (NE.new current))

        ReturnedNavigator current _ ->
            TerminalNavigator next (BackStack (NE.new current))

        MiddleNavigator current (BackStack backNeList) _ ->
            TerminalNavigator next (BackStack (NE.append current backNeList))

        TerminalNavigator current (BackStack backNeList) ->
            TerminalNavigator next (BackStack (NE.append current backNeList))


getCurrent : Navigator a -> a
getCurrent navi =
    case navi of
        InitialNavigator current ->
            current

        ReturnedNavigator current _ ->
            current

        MiddleNavigator current _ _ ->
            current

        TerminalNavigator current _ ->
            current


back : Navigator a -> Navigator a
back navi =
    case navi of
        MiddleNavigator current backStack (ForwardStack forwardStack) ->
            createBackNavigator current backStack (ForwardStack (NE.append current forwardStack))

        TerminalNavigator current backStack ->
            createBackNavigator current backStack (ForwardStack (NE.new current))

        _ ->
            navi


createBackNavigator : a -> BackStack a -> ForwardStack a -> Navigator a
createBackNavigator current (BackStack backStack) forwardStack =
    case NE.tail backStack of
        Just newBackStack ->
            MiddleNavigator (NE.head backStack) (BackStack newBackStack) forwardStack

        Nothing ->
            ReturnedNavigator (NE.head backStack) forwardStack


forward : Navigator a -> Navigator a
forward navi =
    case navi of
        ReturnedNavigator current forwardStack ->
            createForwardNavigator current (BackStack (NE.new current)) forwardStack

        MiddleNavigator current (BackStack backNeList) forwardStack ->
            createForwardNavigator current (BackStack (NE.append current backNeList)) forwardStack

        _ ->
            navi


createForwardNavigator : a -> BackStack a -> ForwardStack a -> Navigator a
createForwardNavigator current backStack (ForwardStack forwardNeList) =
    case NE.tail forwardNeList of
        Just newForwardNeList ->
            MiddleNavigator (NE.head forwardNeList) backStack (ForwardStack newForwardNeList)

        Nothing ->
            TerminalNavigator (NE.head forwardNeList) backStack
