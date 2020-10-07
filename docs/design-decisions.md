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
