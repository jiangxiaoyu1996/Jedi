package context

import expression._

class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name) 