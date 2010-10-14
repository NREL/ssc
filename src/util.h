#ifndef __cpputil_h
#define __cpputil_h

#include <string>
#include <vector>
#include <cassert>

#if defined(_DEBUG) && defined(__VISUALC__) && defined(__32BIT__)
#define VEC_ASSERT(x) {if(!(x)) _asm{int 0x03}}
#else
#define VEC_ASSERT(X) assert(X)
#endif

#define RCINDEX(arr, ncols, r, c) arr[ncols*r+c]

namespace util
{
	std::vector< std::string > split( const std::string &str, const std::string &delim, bool ret_empty=false, bool ret_delim=false );
	std::string join( const std::vector< std::string > &list, const std::string &delim );
		
	bool to_integer(const std::string &str, int *x);
	bool to_float(const std::string &str, float *x);
	bool to_double(const std::string &str, double *x);
		
	std::string to_string( int x, const char *fmt="%d" );
	std::string to_string( double x, const char *fmt="%lg" );

	std::string lower_case( const std::string &in );
	std::string upper_case( const std::string &in );
	
	std::string format(const char *fmt, ...);
	size_t format_vn(char *buffer, int maxlen, const char *fmt, va_list arglist);
	
	template< typename T >
	class matrix_t
	{
	private:
		T *t_array;
		size_t n_rows, n_cols;
	public:

		matrix_t()
		{
			t_array = new T[1];
			n_rows = n_cols = 1;
		}
		
		matrix_t(size_t len)
		{
			t_array = NULL;
			if (len < 1) len = 1;
			resize( 1, len );
		}

		matrix_t(size_t nr, size_t nc)
		{
			t_array = NULL;
			if (nr < 1) nr = 1;
			if (nc < 1) nc = 1;
			resize(nr,nc);
		}
		
		matrix_t(size_t nr, size_t nc, const T &val)
		{
			t_array = NULL;
			if (nr < 1) nr = 1;
			if (nc < 1) nc = 1;
			resize(nr,nc);
			fill(val);
		}


		virtual ~matrix_t()
		{
			if (t_array) delete [] t_array;
		}
		
		void clear()
		{
			if (t_array) delete [] t_array;
			n_rows = n_cols = 0;
		}
		
		void copy( const matrix_t &rhs )
		{
			if (this != &rhs)
			{
				resize( rhs.nrows(), rhs.ncols() );
				size_t nn = n_rows*n_cols;
				for (size_t i=0;i<nn;i++)
					t_array[i] = rhs.t_array[i];
			}
		}

		void assign( const T *pvalues, size_t len )
		{
			resize( len );
			if ( n_cols == len && n_rows == 1 )
				for (size_t i=0;i<len;i++)
					t_array[i] = pvalues[i];
		}
		
		void assign( const T *pvalues, size_t nr, size_t nc )
		{
			resize( nr, nc );
			if ( n_rows == nr && n_cols == nc )
			{
				size_t len = nr*nc;
				for (size_t i=0;i<len;i++)
					t_array[i] = pvalues[i];
			}
		}

		matrix_t &operator=(const matrix_t &rhs)
		{
			copy( rhs );
			return *this;
		}
		
		matrix_t &operator=(const T &val)
		{
			resize(1,1);
			t_array[0] = val;
			return *this;
		}
		
		inline operator T()
		{
			return t_array[0];
		}
		
		bool equals( const matrix_t & rhs )
		{
			if (n_rows != rhs.n_rows || n_cols != rhs.n_cols)
				return false;
			
			size_t nn = n_rows*n_cols;
			for (size_t i=0;i<nn;i++)
				if (t_array[i] != rhs.t_array[i])
					return false;
			
			return true;
		}
		
		inline bool is_single()
		{
			return (n_rows == 1 && n_cols == 1);
		}
			
		inline bool is_array()
		{
			return (n_rows == 1);
		}
		
		void fill( const T &val )
		{
			size_t ncells = n_rows*n_cols;
			for (size_t i=0;i<ncells;i++)
				t_array[i] = val;
		}

		void resize(size_t nr, size_t nc)
		{
			if (nr < 1 || nc < 1) return;
			if (nr == n_rows && nc == n_cols) return;
			
			if (t_array) delete [] t_array;
			t_array = new T[ nr * nc ];
			n_rows = nr;
			n_cols = nc;
		}

		void resize_fill(size_t nr, size_t nc, const T &val)
		{
			resize( nr, nc );
			fill( val );
		}
		
		void resize(size_t len)
		{
			resize( 1, len );
		}
		
		void resize_fill(size_t len, const T &val)
		{
			resize_fill( 1, len, val );
		}
		
		inline T &at(size_t r, size_t c)
		{
	#ifdef _DEBUG
			VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
	#endif
			return t_array[n_cols*r+c];
		}

		inline const T &at(size_t r, size_t c) const
		{
	#ifdef _DEBUG
			VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
	#endif
			return t_array[n_cols*r+c];
		}
		
		T operator[] (size_t i) const
		{
	#ifdef _DEBUG
			VEC_ASSERT( i >= 0 && i < n_cols );
	#endif
			return t_array[i];
		}
		
		T &operator[] (size_t i)
		{
	#ifdef _DEBUG
			VEC_ASSERT( i >= 0 && i < n_cols );
	#endif
			return t_array[i];
		}
				
		inline size_t nrows() const
		{
			return n_rows;
		}
		
		inline size_t ncols() const
		{
			return n_cols;
		}
		
		inline size_t ncells() const
		{
			return n_rows*n_cols;
		}
		
		inline size_t membytes() const
		{
			return n_rows*n_cols*sizeof(T);
		}
		
		void size(size_t &nr, size_t &nc) const
		{
			nr = n_rows;
			nc = n_cols;
		}
		
		size_t length() const
		{
			return n_cols;
		}
		
		inline T *data()
		{
			return t_array;
		}
	};

};


#endif
