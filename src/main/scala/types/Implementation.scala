package types

enum Implementation:
    case Unimplemented(t: Type)
    case Implemented(t: Type, o: Object)

def getImplementationType(i: Implementation): Type =
    i match {
        case Implementation.Unimplemented(t) => t
        case Implementation.Implemented(t, _) => t
    }