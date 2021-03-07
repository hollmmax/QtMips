#ifndef CACHE_TYPES_H
#define CACHE_TYPES_H

#include <cstdint>

namespace machine {

/**
 * Determiners location of address in single way of cache. This mean, where
 * given addresses should be stored, if present.
 */
struct CacheLocation {
    size_t row;
    size_t col;
    size_t tag;
    size_t byte;
};

/**
 * Single cache line. Appropriate cache block is stored in `data`.
 */
struct CacheLine {
    bool valid, dirty;
    size_t tag;
    std::vector<uint32_t> data;
};

/**
 * This is preferred over bool (write = true|false) for better readability.
 */
enum AccessType { READ, WRITE };

inline const char *to_string(AccessType a) {
    switch (a) {
    case READ: return "READ";
    case WRITE: return "WRITE";
    }
}

} // namespace machine

#endif // CACHE_TYPES_H
