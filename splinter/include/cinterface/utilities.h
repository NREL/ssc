/*
 * This file is part of the SPLINTER library.
 * Copyright (C) 2012 Bjarne Grimstad (bjarne.grimstad@gmail.com).
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#ifndef SPLINTER_UTILITIES_H
#define SPLINTER_UTILITIES_H

#include "data_table.h"
#include "function.h"
#include "cinterface.h"
#include "bspline.h"
#include "knot_builders.h"
#include <set>


namespace SPLINTER
{

// Declare the global variables for use in all source files
// All extern variables are defined in cinterface/utilities.cpp
// Keep a list of objects so we avoid performing operations on objects that don't exist
extern std::set<splinter_obj_ptr> datatables;
extern std::set<splinter_obj_ptr> bsplines;

extern int splinter_last_func_call_error; // Tracks the success of the last function call
extern const char *splinter_error_string; // Error string (if the last function call resulted in an error)

void set_error_string(const char *new_error_string);

/* Check for existence of datatable_ptr, then cast splinter_obj_ptr to a DataTable * */
DataTable *get_datatable(splinter_obj_ptr datatable_ptr);

/* Check for existence of bspline_ptr, then cast splinter_obj_ptr to a BSpline * */
BSpline *get_bspline(splinter_obj_ptr bspline_ptr);

// Convert int to Smoothing
BSpline::Smoothing resolve_smoothing(int smoothing);

// Convert int to KnotSpacing
KnotSpacing resolve_knot_spacing(int knot_spacing);

/**
 * Convert from column major to row major with point_dim number of columns.
 *
 * @param col_major Column major data
 * @param point_dim Dimension of each point (= number of columns)
 * @return col_major data stored row major.
 */
double *get_row_major(double *col_major, size_t point_dim, size_t x_len);

/**
 * Convert from standard C array to DenseVector.
 *
 * @param x C array to convert from.
 * @param x_dim The size of x.
 * @return DenseVector with the same data as x.
 */
template <class NUMERICAL_TYPE>
DenseVector get_densevector(NUMERICAL_TYPE *x, size_t x_dim)
{
    DenseVector xvec(x_dim);
    for (size_t i = 0; i < x_dim; i++)
    {
        xvec(i) = (double) x[i];
    }

    return xvec;
}

/**
 * Convert from DenseVector to a vector of NUMERICAL_TYPE.
 * It must be possible to cast from double to NUMERICAL_TYPE.
 *
 * @param x DenseVector to convert from.
 * @return Vector with the same data as x.
 */
template <class NUMERICAL_TYPE>
std::vector<NUMERICAL_TYPE> get_vector(DenseVector x)
{
    auto vector = std::vector<NUMERICAL_TYPE>(x.size());
    for (size_t i = 0; i < x.size(); ++i)
    {
        vector.at(i) = (NUMERICAL_TYPE) x(i);
    }

    return vector;
}

/**
 * Convert from pointer to NUMERICAL_TYPE to std::vector<NUMERICAL_TYPE>
 *
 * @param array Pointer to NUMERICAL_TYPE
 * @param n Number of elements to copy
 * @return std::vector<NUMERICAL_TYPE> with the same elements as in array
 */
template <typename NUMERICAL_TYPE>
std::vector<NUMERICAL_TYPE> get_vector(NUMERICAL_TYPE *array, int n)
{
    return std::vector<NUMERICAL_TYPE>(array, array + n);
}

/**
 * Convert from pointer to NUMERICAL_TYPE to std::vector<std::vector<NUMERICAL_TYPE>>
 * Number of values copied: num_per_row[0] * num_per_row[1] * ... * num_per_row[num_rows - 1]
 *
 * @param array Pointer to NUMERICAL_TYPE
 * @param num_per_row Number of elements per row
 * @param num_rows Number of rows
 * @return std::vector<NUMERICAL_TYPE> with the same elements as in array
 */
template <typename NUMERICAL_TYPE>
std::vector<std::vector<NUMERICAL_TYPE>> get_vector_vector(NUMERICAL_TYPE *array, unsigned int *num_per_row,
                                                           unsigned int num_rows)
{
    auto num_per_row_as_vec = std::vector<unsigned int>(num_per_row, num_per_row + num_rows);

    auto vec_vec = std::vector<std::vector<NUMERICAL_TYPE>>(num_rows);

    int k = 0;
    for (unsigned int i = 0; i < num_rows; ++i)
    {
        unsigned int num_row_i = num_per_row_as_vec.at(i);
        std::vector<NUMERICAL_TYPE> vec(num_row_i);
        for (unsigned int j = 0; j < num_row_i; ++j)
        {
            vec.at(j) = array[k];
            k++;
        }
        vec_vec.at(i) = vec;
    }

    return vec_vec;
}

/**
 * Convert from pointer to NUMERICAL_TYPE to std::vector<std::vector<NUMERICAL_TYPE>>
 * Number of values copied: num_per_row*num_rows
 *
 * @param array Pointer to NUMERICAL_TYPE
 * @param num_per_row Number of elements per row (constant)
 * @param num_rows Number of rows
 * @return std::vector<NUMERICAL_TYPE> with the same elements as in array
 */
template <typename NUMERICAL_TYPE>
std::vector<std::vector<NUMERICAL_TYPE>> get_vector_vector(NUMERICAL_TYPE *array, unsigned int num_per_row,
                                                           unsigned int num_rows)
{
    auto vec_vec = std::vector<std::vector<NUMERICAL_TYPE>>(num_rows);

    unsigned int k = 0;
    for (unsigned int i = 0; i < num_rows; ++i)
    {
        std::vector<NUMERICAL_TYPE> vec(num_per_row);
        for (unsigned int j = 0; j < num_per_row; ++j)
        {
            vec.at(j) = array[k];
            k++;
        }
        vec_vec.at(i) = vec;
    }

    return vec_vec;
}

} // namespace SPLINTER

#endif // SPLINTER_UTILITIES_H
