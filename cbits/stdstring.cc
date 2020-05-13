#include <string.h>
#include <iostream>

extern "C" {

  size_t string_get_length(const std::string *s) {
    return s->length();
  }

  void string_delete(std::string *s) {
    delete s;
  }
  
  void string_copy(const std::string s, char* out) {
    s.copy(out, s.length());
  }
  
}
