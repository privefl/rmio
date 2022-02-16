#ifndef RMIO_CREATE_FILE_HPP
#define RMIO_CREATE_FILE_HPP

/******************************************************************************/

// /!\ This is bullshit, and will get removed in the future /!\
// Use instead the R functions provided here

/******************************************************************************/

#include <fstream>
#include <stdexcept>

/******************************************************************************/

inline void create_file(const char * filename,
                        std::size_t n_elem,
                        std::size_t type_size) {

  try {
    std::filebuf fbuf;
    fbuf.open(filename, std::ios_base::out | std::ios_base::binary);
    fbuf.pubseekpos(n_elem * type_size - 1); fbuf.sputc(0);
    fbuf.close();
  } catch(std::exception& ex) {
    throw std::runtime_error("Problem creating the file.");
  }
}

/******************************************************************************/

inline void append_file(const char * filename,
                        std::size_t n_elem,
                        std::size_t type_size) {

  try {
    std::fstream filestr(filename);
    std::streambuf * pbuf = filestr.rdbuf();
    pbuf->pubseekoff(n_elem * type_size - 1, filestr.end); pbuf->sputc(0);
    filestr.close();
  } catch(std::exception& ex) {
    throw std::runtime_error("Problem resizing the file.");
  }
}

/******************************************************************************/

#endif // RMIO_CREATE_FILE_HPP
