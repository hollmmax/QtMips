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

#include "cache_policy.h"
#include "../../qtmipsexception.h"

namespace machine {

CachePolicy *CachePolicy::get_policy_instance(const CacheConfig *config) {
    switch (config->replacement_policy()) {
        case CacheConfig::RP_RAND:
            return new CachePolicyRAND(config->associativity());
        case CacheConfig::RP_LRU:
            return new CachePolicyLRU(config->associativity(), config->set_count());
        case CacheConfig::RP_LFU:
            return new CachePolicyLFU(config->associativity(), config->set_count());
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

void CachePolicyLRU::update_stats(
    size_t assoc_index,
    size_t set_index,
    bool is_valid
) {
    uint32_t next_asi = assoc_index;

    if (is_valid) {
        ssize_t i = stats.size() - 1;
        while (stats[set_index][i] != assoc_index) {
            SANITY_ASSERT(i >= 0, "LRU lost the way from priority queue - access");
            std::swap(stats[set_index][i], next_asi);
            i--;
        }
    } else {
        size_t i = 0;
        while (stats[set_index][i] != assoc_index) {
            SANITY_ASSERT(i < stats.size(), "LRU lost the way from priority queue - access");
            std::swap(stats[set_index][i], next_asi);
            i++;
        }
    }
}

size_t CachePolicyLRU::select_index_to_evict(size_t set_index) const {
    return stats[set_index][0];
}


void CachePolicyLFU::update_stats(
    size_t assoc_index,
    size_t set_index,
    bool is_valid
) {
    auto &stat_item = stats[set_index][assoc_index];

    if (is_valid) {
        stat_item += 1;
    } else {
        stat_item = 0;
    }
}

size_t CachePolicyLFU::select_index_to_evict(size_t set_index) const {
    size_t lowest = stats[set_index][0];
    size_t index = 0;
    for (size_t i = 1; i < stats.size(); i++) {
        if (stats[set_index][i] == 0) {
            // Only invalid blocks have zero stat
            return i;
        }
        if (lowest > stats[set_index][i]) {
            lowest = stats[set_index][i];
            index = i;
        }
    }
    return index;
}

CachePolicyLFU::CachePolicyLFU(
    size_t associativity,
    size_t set_count
) {
    stats.resize(associativity, std::vector<uint32_t>(set_count, 0));
}

void CachePolicyRAND::update_stats(
    size_t assoc_index,
    size_t set_index,
    bool is_valid
) {
    // NOP
}

size_t CachePolicyRAND::select_index_to_evict(size_t set_index) const {
    return std::rand() % associativity;
}

}