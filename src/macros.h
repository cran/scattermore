/*
 * This file is part of scattermore.
 *
 * Copyright (C) 2022-2023 Mirek Kratochvil <exa.exa@gmail.com>
 *               2022-2023 Tereza Kulichova <kulichova.t@gmail.com>
 *
 * scattermore is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * scattermore is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * scattermore. If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef MACROS_H
#define MACROS_H

#include <cstddef>
#include <limits>

inline size_t
f2i(float x)
{
  if (x < 0 || x > float(std::numeric_limits<size_t>::max()))
    return std::numeric_limits<size_t>::max();
  return size_t(x);
}

#endif
