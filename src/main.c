#include <stddef.h>
#include "lexer.h"
#include "type.h"

/* Externs */

Type *string_type;
Type *integer_type;
Type_Ptr_list type_table = {0};

/* End Externs */

int main(int argc, char *argv[]) {
  string_type = type_alloc();
  *string_type = (Type) {
    .index = 0,
    .name = "string"
  };

  integer_type = type_alloc();
  *string_type = (Type) {
    .index = 1,
    .name = "int"
  };

  LIST_PUSH(type_table, string_type);
  LIST_PUSH(type_table, integer_type);
 
  Lexer lexer;
  lexer_init(&lexer, "input.bi");

  while (true) {
    Token tok = lexer_gettok(&lexer);

    printf("got token: %d..\n", tok.type);

    if (tok.has_value) {
      printf("\t\"%s\"\n", tok.value);
    }

    if (tok.type == TOKEN_EOF) {
      break;
    }
  }

  return 0;
}