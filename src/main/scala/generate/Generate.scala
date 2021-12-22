package generate

import parse.ast.*

def generateHTML(s: Template, obj: types.Object = null): String = 
  val head = generateHead
  val body = generateBody(s, obj)
  s"""<!DOCTYPE html>
    <html>
      <head>
        $head
      </head>
      <body>
        $body
      </body>
  </html>"""