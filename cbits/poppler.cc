#include <poppler-document.h>
#include <poppler-page.h>
#include <iostream>
#include <string.h>

extern "C" {

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

  poppler::page* poppler_document_open_page(poppler::document* doc, int page) {
    return doc->create_page(page);
  }

  std::string* poppler_page_text(poppler::page* page, int layout) {
    std::vector<char> vc;
    switch (layout) {
    case 0: { 
      vc = page->text(poppler::rectf(), poppler::page::text_layout_enum::raw_order_layout).to_utf8();
      break;
    }
    case 1: {
      vc = page->text(poppler::rectf(), poppler::page::text_layout_enum::physical_layout).to_utf8();
      break;
    }
    default: {
      vc = page->text(poppler::rectf(), poppler::page::text_layout_enum::non_raw_non_physical_layout).to_utf8();
      break;
    }
    }
    return new std::string(vc.begin(), vc.end());
  }

  void poppler_page_delete(poppler::page* page) {
    delete page;
  }

  

}
