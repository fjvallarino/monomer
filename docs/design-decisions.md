Why do you use hidden internal state instead of having the user define the hidden type explicitly in their model?
Why, except for render, is the widget interface non monadic?
Why is the return type of render IO and not some custom monad?
Why not FRP?
Why Lens and not MicroLens?
Why does Widget have a reference to children widget, considering the Widget may not have any?
Why records of functions instead of typeclasses?
Why functional dependencies instead of type families for widget combinators?
Why do you use Lens style record field names for internal config types?
Why do you use lawless typeclasses for combinators?
Why isn't StyleState using Phantom Types? Those styleX functions could be made safer.
Why did you add themes, considering you can easily create a customized version of a widget by just writing a function and using it across the application?
Why did you remove Margin?
Why did you switch to returning Maybe from rect/size operations? Revise addOuterSizeReq (word this question properly before answering)
Why does exit event works this way? Why not use dispose? Talk about the need for broadcast, the intention of not providing more events to widgets than needed, etc
Why do containers have the optional config argument before their children? It's inconsistent with where single widgets have it.
