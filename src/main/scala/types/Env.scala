package types

import parse.ast.Identifier

class Env(var mapping: Map[Identifier, Object] = Map())