module NonEmptyList exposing (NonEmptyList, append, head, new, tail)


type NonEmptyList a
    = NonEmptyList (List a) a


new : a -> NonEmptyList a
new item =
    NonEmptyList [] item


append : a -> NonEmptyList a -> NonEmptyList a
append item (NonEmptyList items base) =
    NonEmptyList (item :: items) base


head : NonEmptyList a -> a
head (NonEmptyList items base) =
    case items of
        x :: xs ->
            x

        [] ->
            base


tail : NonEmptyList a -> Maybe (NonEmptyList a)
tail (NonEmptyList items base) =
    case items of
        x :: xs ->
            Just (NonEmptyList xs base)

        [] ->
            Nothing
