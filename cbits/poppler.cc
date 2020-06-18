#include <poppler-document.h>
#include <poppler-page.h>
#include <iostream>
#include <string.h>

/*

Failed attempt to make poppler quiet
====================================



#include "Error.h"
#include "GlobalParams.h"

static void errorcb(ErrorCategory, Goffset pos, const char *msg) {
  ...
}

setErrorCallback(errorcb);
globalParams = std::make_unique<GlobalParams>();
globalParams->setErrQuiet(true);



Do not forget to add poppler to pkgconfig-depends.

Neither setting error callback nor setting global param makes
poppler quiet. It seems that the errQuiet flag is set
and the changes are visible inside poppler.cc, however
they seem not to be visible by poppler library. I probably
do something wrong.

*/

extern "C" {

  std::string* to_stdstring(poppler::ustring ust) {
    std::vector<char> vc = ust.to_utf8();
    return new std::string(vc.begin(), vc.end());
  }
  
  poppler::document* poppler_document_open_pdf(const char* file) {
    poppler::document* doc = poppler::document::load_from_file(file);
    return doc;
  }

  poppler::document* poppler_document_open_data(const char* data, size_t n) {
    std::vector<char> d;
    d.assign(data, data + n);
    return poppler::document::load_from_data(&d);
  }

  void poppler_document_delete(poppler::document* doc) {
    delete doc;
  }

  int poppler_document_pages(poppler::document* doc) {
    return doc->pages();
  }

  std::string* poppler_document_author(poppler::document* doc) {
    return to_stdstring(doc->get_author());
  }

  std::string* poppler_document_creator(poppler::document* doc) {
    return to_stdstring(doc->get_creator());
  }

  std::string* poppler_document_producer(poppler::document* doc) {
    return to_stdstring(doc->get_producer());
  }

  std::string* poppler_document_subject(poppler::document* doc) {
    return to_stdstring(doc->get_subject());
  }

  std::string* poppler_document_title(poppler::document* doc) {
    return to_stdstring(doc->get_title());
  }

  std::string* poppler_document_keywords(poppler::document* doc) {
    return to_stdstring(doc->get_keywords());
  }

  std::string* poppler_document_metadata(poppler::document* doc) {
    return to_stdstring(doc->metadata());
  }

  poppler::page* poppler_document_open_page(poppler::document* doc, int page) {
    return doc->create_page(page);
  }

  std::string* poppler_page_text(poppler::page* page, int layout) {
    std::vector<char> vc;
    switch (layout) {
    case 0: { 
      return to_stdstring(page->text(poppler::rectf(), poppler::page::text_layout_enum::raw_order_layout));
    }
    case 1: {
      return to_stdstring(page->text(poppler::rectf(), poppler::page::text_layout_enum::physical_layout));
    }
    default: {
      return to_stdstring(page->text(poppler::rectf(), poppler::page::text_layout_enum::non_raw_non_physical_layout));
    }
    }
  }

  void poppler_page_delete(poppler::page* page) {
    delete page;
  }

  

}
