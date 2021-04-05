/*
 * This file is part of fixed point library.
 *
 * Developed for the under-byte fixed point arithmetic emulation purpose.
 * This software developmend had been supported by Koh Young Technology Inc.
 * (https://www.kohyoung.com).
 * See the COPYRIGHT file at the top-level directory of this distribution
 * for details of code ownership.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <algorithm>
 
#pragma warning( disable : 4244 )
#pragma warning( disable : 4293 )

#ifndef _FIXED_POINT
#define _FIXED_POINT

#define SIGNED   true
#define UNSIGNED false

template <unsigned int Integer, unsigned int Fraction>
using ufixed_container = std::conditional_t<Integer + Fraction <= 8, std::uint8_t, 
    std::conditional_t<Integer + Fraction <= 16, std::uint16_t, 
    std::conditional_t<Integer + Fraction <= 32, std::uint32_t, std::uint64_t>>
>;

template <unsigned int Integer, unsigned int Fraction>
using sfixed_container = std::conditional_t<1 + Integer + Fraction <= 8, std::int8_t, 
    std::conditional_t<1 + Integer + Fraction <= 16, std::int16_t, 
    std::conditional_t<1 + Integer + Fraction <= 32, std::int32_t, std::int64_t>>
>;

template <bool Signed, unsigned int Integer, unsigned int Fraction>
struct fixed{
    std::conditional_t<Signed, 
        sfixed_container<Integer, Fraction>, 
        ufixed_container<Integer, Fraction>> container;

    // Compile-time determined values
    static constexpr std::uint8_t  Reserved = sizeof(container) * 8 - Integer - Fraction - Signed;
    static constexpr std::uint64_t unit  = 1<<(Reserved);
    static constexpr double        fbase = (1.0 / (1<<Fraction));

    // Cast to fixed
    template<bool cSigned, unsigned int cInteger, unsigned int cFraction>
    operator fixed<cSigned, cInteger, cFraction>()
    {
        fixed<cSigned, cInteger, cFraction> ret;
        constexpr auto          cReserved = sizeof(ret.container) * 8 - cInteger - cFraction - cSigned;
        constexpr std::int64_t  bit_shift = ((std::int64_t)Fraction + Reserved) - ((std::int64_t)cFraction + cReserved);

        if(bit_shift > 0) ret.container = (decltype(ret.container))(this->container >> bit_shift);
        else              ret.container = (decltype(ret.container))(this->container <<-bit_shift);

        return ret;
    };

    fixed(){this->container = 0;}
    template<typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
    fixed(const T val){*this = val;}
    
    template<bool cSigned, unsigned int cInteger, unsigned int cFraction>
    decltype(auto) operator = (const fixed<cSigned, cInteger, cFraction> val)
    {
        fixed<cSigned, cInteger, cFraction> _val = val;
        *this = (fixed<Signed, Integer, Fraction>) _val;
    }

    template<typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
    decltype(auto) operator = (const T val)
    {
        if constexpr (std::is_floating_point_v<T>) this->container = ((std::uint64_t)(val / fbase)) << Reserved;
        else if constexpr  (std::is_integral_v<T>) this->container = ((std::uint64_t)(val)) << (Reserved + Fraction);
        return *this;
    }

    template<typename T, typename = std::enable_if_t<std::is_arithmetic_v<std::decay_t<T>>>>
    operator T()
    {
        return (T) ((this->container >> Reserved) * fbase);
    }  
    
};


template <bool Signed,unsigned int Integer, unsigned int Fraction, typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
decltype(auto) operator + (fixed<Signed, Integer, Fraction> lhs, T val)
{
    fixed<Signed, Integer, Fraction> fixed_val = val;
    return lhs + fixed_val;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator + (fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   = lSigned | rSigned;
    constexpr auto cInteger  = std::max(lInteger, rInteger) + 1;
    constexpr auto cFraction = std::max(lFraction, rFraction);

    fixed<cSigned, cInteger, cFraction> exp_lhs = lhs;
    fixed<cSigned, cInteger, cFraction> exp_rhs = rhs;
    fixed<cSigned, cInteger, cFraction> ret;

    ret.container = exp_lhs.container + exp_rhs.container;
    return ret;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator - (fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   = SIGNED;
    constexpr auto cInteger  = std::max(lInteger, rInteger) + lSigned|rSigned;
    constexpr auto cFraction = std::max(lFraction, rFraction);

    fixed<cSigned, cInteger, cFraction> exp_lhs = lhs;
    fixed<cSigned, cInteger, cFraction> exp_rhs = rhs;
    fixed<cSigned, cInteger, cFraction> ret;

    ret.container = exp_lhs.container - exp_rhs.container;
    return ret;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator * (fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   = lSigned | rSigned;
    constexpr auto cInteger  = ((lInteger == 1 && lFraction == 0)?0:lInteger) + ((rInteger == 1 && rFraction == 0)?0:rInteger);
    // one bit integer could not change fixed point size
    // do not change size if lhs or rhs is fixed<ANY, 1, 0>
    constexpr auto cFraction = lFraction + rFraction;

    fixed<cSigned, cInteger, cFraction> ret;

    constexpr auto lReserved = sizeof(lhs.container) * 8 - lInteger - lFraction - lSigned;
    constexpr auto rReserved = sizeof(rhs.container) * 8 - rInteger - rFraction - rSigned;
    constexpr auto cReserved = sizeof(ret.container) * 8 - cInteger - cFraction - cSigned;

    ret.container = ((lhs.container>>lReserved) * (rhs.container>>rReserved))<<cReserved;
    return ret;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator / (fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   =   lSigned | rSigned;
    constexpr auto cInteger  =  lInteger + rFraction;
    constexpr auto cFraction = lFraction + rInteger ;

    constexpr auto lReserved = sizeof(lhs.container) * 8 - lSigned - lInteger - lFraction - lSigned;
    constexpr auto rReserved = sizeof(rhs.container) * 8 - rSigned - rInteger - rFraction - rSigned;

    fixed<cSigned, cInteger, cFraction> ret;
    constexpr auto cReserved = sizeof(ret.container) * 8 - cSigned - cInteger - cFraction ;

    auto num   = (decltype(ret.container))lhs.container>>lReserved;
    num = num << (rInteger + rFraction);
    auto denom = (decltype(ret.container))rhs.container>>rReserved;
    if(denom != 0)
    {
        ret.container = (num / denom)<<cReserved;
    }
    else
    {
        ret.container = 0;
    }
    

    return ret;
}

template<bool Signed, unsigned int Integer, unsigned int Fraction>
decltype(auto) operator <<(std::ostream& os, const fixed<Signed, Integer, Fraction>& rhs)
{
    auto _rhs = rhs; 
    os<<(double)_rhs;
    return os;
}

template<bool Signed, unsigned int Integer, unsigned int Fraction>
decltype(auto) operator <<(std::ofstream& os, fixed<Signed, Integer, Fraction>& rhs)
{
    auto _rhs = rhs; 

    constexpr auto Reserved = sizeof(rhs.container) * 8- Signed - Integer - Fraction ;
    std::make_unsigned_t<decltype(rhs.container)> val = (static_cast<std::make_unsigned_t<decltype(rhs.container)>>(rhs.container))>>Reserved;
    std::stringstream stream;
    constexpr auto size = ((Signed + Integer + Fraction) / 4) + ((Signed + Integer + Fraction) % 4 ? 1 : 0);
    if constexpr(size < 3)
    {
        auto _val = static_cast<std::uint16_t>(val);
        stream << std::setfill ('0') << std::setw(size)<< std::hex << _val;
    }
    else{
        stream << std::setfill ('0') << std::setw(size)<< std::hex << val;
    }
    os << stream.str() << std::endl;
    return os;
}

std::ofstream& operator <<(std::ofstream& os, float v)
{
    std::uint32_t* pval = (std::uint32_t*)(&v);
    std::stringstream stream;
    stream << std::setfill ('0') << std::setw(8)<< std::hex << *(pval);
    os << stream.str() << std::endl;
    return os;
}

bool operator &&(const fixed<SIGNED, 1, 0> lhs, const fixed<SIGNED, 1, 0> rhs)
{
    return lhs.container && rhs.container;
}

template<bool Signed, unsigned int Integer, unsigned int Fraction, typename=std::enable_if_t<Signed>>
decltype(auto) abs(const fixed<Signed, Integer, Fraction>& value)
{
    if constexpr(Signed)
    {
        auto value_abs = value;
        value_abs.container = value_abs.container >= 0 ? value_abs.container : -value_abs.container;
        fixed<UNSIGNED, Integer, Fraction> ret = value_abs;
        return ret;
    }
}
template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction, typename F>
decltype(auto) comp(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs, F func)
{
    constexpr auto cSigned   = lSigned | rSigned;
    constexpr auto cInteger  = std::max(lInteger, rInteger);
    constexpr auto cFraction = std::max(lFraction, rFraction);

    const fixed<cSigned, cInteger, cFraction> exp_lhs = lhs;
    const fixed<cSigned, cInteger, cFraction> exp_rhs = rhs;

    return func(exp_lhs.container, exp_rhs.container);
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator >(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs)
{
    return comp(lhs, rhs, [](auto a, auto b){return a > b;});
}
template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator <(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs)
{
    return comp(lhs, rhs, [](auto a, auto b){return a < b;});
}
template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator >=(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs)
{
    return comp(rhs, lhs, [](auto a, auto b){return a >= b;});
}
template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator <=(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs)
{
    return comp(rhs, lhs, [](auto a, auto b){return a <= b;});
}
template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) operator ==(const fixed<lSigned, lInteger, lFraction>& lhs, const fixed<rSigned, rInteger, rFraction>& rhs)
{
    return comp(rhs, lhs, [](auto a, auto b){return a == b;});
}


/**
 * Cast fixed point with unsafe manner.
 *
 * This method just move container value to destination object.
 * with differentate Integer length, this method may be used as divider or multiplier
 * (e.g  fixed<UNSIGNED,2,2> ==unsafe cast=> fixed<UNSIGNED,1,3> is identical to /2)
 * (e.g2 fixed<UNSIGNED,2,2> ==unsafe cast=> fixed<UNSIGNED,3,1> is identical to *2)
 * 
 * @param val: original fixed point to be casted
 * @return ret: destination fixed point 
 * 
 */
template <bool cSigned, unsigned int cInteger, unsigned int cFraction, bool oSigned, unsigned int oInteger, unsigned int oFraction>
decltype(auto) unsafe_cast(fixed<oSigned, oInteger, oFraction> val)
{
    fixed<cSigned, cInteger, cFraction> ret;
    ret.container = val.container;
    return ret;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) unsafe_add(fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   = lSigned | rSigned;
    constexpr auto cInteger  = std::max(lInteger, rInteger);
    constexpr auto cFraction = std::max(lFraction, rFraction);

    fixed<cSigned, cInteger, cFraction> exp_lhs = lhs;
    fixed<cSigned, cInteger, cFraction> exp_rhs = rhs;
    fixed<cSigned, cInteger, cFraction> ret;

    ret.container = exp_lhs.container + exp_rhs.container;
    return ret;
}

template <bool lSigned, unsigned int lInteger, unsigned int lFraction, bool rSigned, unsigned int rInteger, unsigned int rFraction>
decltype(auto) unsafe_sub(fixed<lSigned, lInteger, lFraction> lhs, fixed<rSigned, rInteger, rFraction> rhs)
{
    constexpr auto cSigned   = SIGNED;
    constexpr auto cInteger  = std::max(lInteger, rInteger);
    constexpr auto cFraction = std::max(lFraction, rFraction);

    fixed<cSigned, cInteger, cFraction> exp_lhs = lhs;
    fixed<cSigned, cInteger, cFraction> exp_rhs = rhs;
    fixed<cSigned, cInteger, cFraction> ret;

    ret.container = exp_lhs.container - exp_rhs.container;
    return ret;
}


template<int Shift, bool Signed, unsigned int Integer, unsigned int Fraction>
decltype(auto) modify_point(const fixed<Signed, Integer, Fraction>& val)
{
    fixed<Signed, Integer + Shift, Fraction - Shift> ret;
    ret.container = val.container;
    return ret;
}

#endif
