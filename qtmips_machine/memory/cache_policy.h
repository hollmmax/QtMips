// SPDX-License-Identifier: GPL-2.0+
/*******************************************************************************
 * QtMips - MIPS 32-bit Architecture Subset Simulator
 *
 * Implemented to support following courses:
 *
 *   B35APO - Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b35apo
 *
 *   B4M35PAP - Advanced Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b4m35pap/start
 *
 * Copyright (c) 2017-2019 Karel Koci<cynerd@email.cz>
 * Copyright (c) 2019      Pavel Pisa <pisa@cmp.felk.cvut.cz>
 * Copyright (c) 2020      Jakub Dupak <dupak.jakub@gmail.com>
 * Copyright (c) 2020      Max Hollmann <hollmmax@fel.cvut.cz>
 *
 * Faculty of Electrical Engineering (http://www.fel.cvut.cz)
 * Czech Technical University        (http://www.cvut.cz/)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301, USA.
 *
 ******************************************************************************/

#ifndef QTMIPS_CACHE_POLICY_H
#define QTMIPS_CACHE_POLICY_H

#include <cstdint>
#include <cstdlib>
#include "cache_types.h"

namespace machine {

class CachePolicy {
public:
    virtual size_t select_index_to_evict(const CacheLocation &loc) const = 0;

    virtual void update_replacement_data(
        size_t index,
        CacheLocation loc,
        bool is_valid
    ) = 0;

    virtual ~CachePolicy() = 0;

    static CachePolicy *get_policy(
        const CacheConfig *config,
        const cache_data *const *const &dt
    );
};


class CachePolicyLRU : public CachePolicy {
public:
    CachePolicyLRU(
        size_t associativity,
        size_t set_count
    );

    size_t select_index_to_evict(const CacheLocation &loc) const override;


    void update_replacement_data(
        size_t index,
        CacheLocation loc,
        bool is_valid
    ) override;

private:
    std::vector<std::vector<uint32_t>> stats;
};


class CachePolicyLFU : public CachePolicy {
public:
    CachePolicyLFU(
        size_t associativity,
        size_t set_count,
        const machine::cache_data *const *const &dt
    );

    size_t select_index_to_evict(const CacheLocation &loc) const override;

    void update_replacement_data(
        size_t index,
        CacheLocation loc,
        bool is_valid
    ) override;

private:
    std::vector<std::vector<uint32_t>> stats;
    const machine::cache_data *const *const &dt;
};



class CachePolicyRAND : public CachePolicy {
public:
    explicit CachePolicyRAND(size_t associativity) : associativity(associativity) {}

private:
    size_t select_index_to_evict(const CacheLocation &loc) const override;

    void update_replacement_data(
        size_t index,
        CacheLocation loc,
        bool is_valid
    ) override;

private:
    size_t associativity;
};

CachePolicy *CachePolicy::get_policy(
    const CacheConfig *config,
    const cache_data *const *const &dt
) {
    switch (config->replacement_policy()) {
        case CacheConfig::RP_RAND:
            return new CachePolicyRAND(config->associativity());
        case CacheConfig::RP_LRU:
            return new CachePolicyLRU(config->associativity(), config.set_count());
        case CacheConfig::RP_LFU:
            return new CachePolicyLFU(config->associativity(), config.set_count(), dt);
    }
}


CachePolicyLRU::CachePolicyLRU(
    size_t associativity,
    size_t set_count
) {
    stats.resize(associativity);
    for (auto row: stats) {
        row.reserve(set_count);
        for (size_t i = 0; i < set_count; i++) {
            row.push_back(i);
        }
    }
}

void CachePolicyLRU::update_replacement_data(
    size_t index,
    CacheLocation loc,
    bool is_valid
) {
    size_t next_asi = index;
    int64_t i = stats.size() - 1;
    size_t tmp_asi = stats[loc.row][i];
    while (tmp_asi != index) {
        SANITY_ASSERT(i >= 0, "LRU lost the way from priority queue - access");
        tmp_asi = stats[loc.row][i];
        stats[loc.row][i] = next_asi;
        next_asi = tmp_asi;
        i--;
    }
}

size_t CachePolicyLRU::select_index_to_evict(const CacheLocation &loc) const {
    return stats[loc.row][0];
}


void CachePolicyLFU::update_replacement_data(
    size_t index,
    CacheLocation loc,
    bool is_valid
) {}

size_t CachePolicyLFU::select_index_to_evict(const CacheLocation &loc) const {
    size_t lowest = stats[loc.row][0];
    size_t index = 0;
    for (size_t i = 1; i < stats.size(); i++) {
        if (!dt[i][loc.row].valid) {
            index = i;
            break;
        }
        if (lowest > stats[loc.row][i]) {
            lowest = stats[loc.row][i];
            index = i;
        }
    }
    return index;
}

CachePolicyLFU::CachePolicyLFU(
    size_t associativity,
    size_t set_count,
    const cache_data *const *const &dt
) : dt(dt) {
    stats.resize(associativity);
    for (auto row: stats) {
        row.reserve(set_count);
        for (size_t i = 0; i < set_count; i++) {
            row.push_back(i);
        }
    }
}

void CachePolicyRAND::update_replacement_data(
    size_t index,
    CacheLocation loc,
    bool is_valid
) {}

size_t CachePolicyRAND::select_index_to_evict(const CacheLocation &loc) const {
    return rand() % associativity;
}

}

#endif //QTMIPS_CACHE_POLICY_H