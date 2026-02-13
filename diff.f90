module diff_demo
    implicit none
contains

    ! primality test (trial division)
    logical function is_prime(n)
        implicit none
        integer, intent(in) :: n
        integer :: i, limit

        if (n < 2) then
            is_prime = .false.
            return
        end if

        limit = int(sqrt(real(n)))
        do i = 2, limit
            if (mod(n, i) == 0) then
                is_prime = .false.
                return
            end if
        end do

        is_prime = .true.
    end function is_prime

    ! pick a random prime in [2, limit-1]
    function get_prime(limit) result(prime)
        implicit none
        integer, intent(in) :: limit
        integer :: prime
        real :: r

        if (limit <= 3) then
            ! smallest sensible prime to return
            prime = 2
            return
        end if

        prime = 1
        do while (.not. is_prime(prime))
            call random_number(r)      ! r in [0,1)
            prime = 2 + int(r * real(limit - 2))   ! in [2, limit-1]
            if (prime < 2) prime = 2
        end do
    end function get_prime

    ! find a generator g for a safe prime p = 2q+1
    function get_generator_g(p) result(g)
        implicit none
        integer, intent(in) :: p
        integer :: g, q
        real :: r

        q = (p - 1) / 2

        do
            call random_number(r)
            g = 2 + int(r * real(p - 2))   ! g in [2, p-1]
            if ( g >= 2 ) then
                if ( mod_pow(g, 2, p) /= 1 .and. mod_pow(g, q, p) /= 1 ) then
                    return
                end if
            end if
        end do
    end function get_generator_g

    ! extended Euclidean algorithm (recursive)
    ! computes gcd(a,b) and x,y such that a*x + b*y = gcd
    recursive subroutine extended_gcd(a, b, gcd, x, y)
        implicit none
        integer, intent(in)  :: a, b
        integer, intent(out) :: gcd, x, y
        integer :: gcd1, x1, y1, a1, b1

        if (a == 0) then
            gcd = b
            x = 0
            y = 1
            return
        end if

        a1 = mod(b, a)
        b1 = a
        call extended_gcd(a1, b1, gcd1, x1, y1)

        gcd = gcd1
        x = y1 - (b / a) * x1
        y = x1
    end subroutine extended_gcd

    ! modular inverse: returns 0 if inverse does not exist
    function mod_inverse(a, m) result(inv)
        implicit none
        integer, intent(in) :: a, m
        integer :: inv
        integer :: gcd, x, y

        call extended_gcd(mod(a, m), m, gcd, x, y)
        if (gcd /= 1) then
            inv = 0
            return
        end if

        inv = mod(x, m)
        if (inv < 0) inv = inv + m
    end function mod_inverse

    ! private key: random integer in [1, q]
    function get_private_key(p) result(key)
        implicit none
        integer, intent(in) :: p
        integer :: key, q
        real :: r

        q = (p - 1) / 2
        if (q < 1) then
            key = 1
            return
        end if

        call random_number(r)
        key = 1 + int(r * real(q))   ! 1 .. q
    end function get_private_key

    ! modular exponentiation (fast pow mod)
    function mod_pow(base, exp, m) result(res)
        implicit none
        integer, intent(in) :: base, exp, m
        integer :: res
        integer :: b, e

        if (m == 1) then
            res = 0
            return
        end if

        res = 1
        b = mod(base, m)
        e = exp

        do while (e > 0)
            if (mod(e, 2) == 1) then
                res = mod(res * b, m)
            end if
            b = mod(b * b, m)
            e = e / 2
        end do
        if (res < 0) then
            res = res + m
        end if
    end function mod_pow

    ! XOR-encrypt a string into an integer array (1-based indexing)
    ! LCG with simple pseudo-random number generator (0..255)
    subroutine encrypt_message(plaintext, key, ciphertext)
        implicit none
        character(len=*), intent(in) :: plaintext
        integer, intent(in) :: key
        integer, allocatable, intent(out) :: ciphertext(:)
        integer :: i, n
        integer :: code, state

        n = len_trim(plaintext)
        allocate(ciphertext(n))

        ! Initialize PRNG state using the key
        state = key

        ! LCG
        do i = 1, n
            state = mod(1664525 * state + 1013904223, 2**32)
            code = ichar(plaintext(i:i))
            ! XOR with the lower byte of the PRNG
            ciphertext(i) = ieor(code, mod(state, 256))
        end do
    end subroutine encrypt_message

        ! decrypt integer array back to a string
        ! reverse LCG
    subroutine decrypt_message(ciphertext, key, plaintext)
        implicit none
        integer, intent(in) :: ciphertext(:)
        integer, intent(in) :: key
        character(len=:), allocatable, intent(out) :: plaintext
        integer :: i, n
        integer :: code, state

        n = size(ciphertext)
        allocate(character(len=n) :: plaintext)

        ! Initialize PRNG state using the key
        state = key

        do i = 1, n
            state = mod(1664525 * state + 1013904223, 2**32)
            code = ieor(ciphertext(i), mod(state, 256))
            plaintext(i:i) = char(code)
        end do
    end subroutine decrypt_message

    end module diff_demo
