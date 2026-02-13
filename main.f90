program main
    use diff_demo
    implicit none

    integer :: bound, prime, g ! Prime bound, p, g
    integer :: priv_a, pub_A, shared_A ! Alice's numbers
    integer :: priv_b, pub_B, shared_B ! Bob's numbers
    character(len=:), allocatable :: message, returned_message ! Plaintexts
    character(len=200) :: temp_input ! Parse input
    integer, allocatable :: ciphertext(:) ! Ciphertext
    integer :: read_status, count_i ! Counters, extra status check
    integer, allocatable :: seed(:) ! Seed for RNG
    integer :: i, n, t

    ! get size of RNG seed array
    call random_seed(size = n)
    allocate(seed(n))

    ! use system clock for varying seed
    call system_clock(count=t)
    do i = 1, n
        seed(i) = t + i*37
    end do

    call random_seed(put = seed)

    print "(A)", "Enter prime upper bound:"
    read (*,*, iostat=read_status) bound

    prime = get_prime(bound)
    print *
    print *
    print "(A,I6,A)", "Alice picks ", prime, " as her prime"
    
    g = get_generator_g(prime)
    print "(A,I6,A)", "Alice picks ", g, " as her generator g"

    priv_a = get_private_key(prime)
    print "(A,I6,A)", "( Alice picks ", priv_a, " as her private key )"

    pub_A = mod_pow(g, priv_a, prime)
    print "(A,I6,A)", "Alice computes g ^ a mod prime = ", pub_A, " as her public key"

    print "(A,I6,A,I6,A,I6,A)", "MESSAGE SENT: Alice sends p = ", prime, " g = ", g, " A = ", pub_A, " to Bob"
    print *
    priv_b = get_private_key(prime)
    print "(A,I6,A)", "( Bob picks ", priv_b, " as his private key )"

    pub_B = mod_pow(g, priv_b, prime)
    print "(A,I6,A)", "Bob computes g ^ b mod prime = ", pub_B, " as his public key"

    shared_B = mod_pow(pub_A, priv_b, prime)
    print "(A,I6,A)", "( Bob computes A ^ b mod prime = ", shared_B, " as his shared key )"

    print "(A,I6,A)", "MESSAGE SENT: Bob sends B = ", pub_B, " to Alice"
    print *
    shared_A = mod_pow(pub_B, priv_a, prime)
    print "(A,I6,A)", "( Alice computes B ^ a mod prime = ", shared_A, " as her shared key )"
    print *
    
    print "(A,I6,A,I6,A,I6,A,I6)", "Public knowledge: p =", prime, " g =", g, " A =", pub_A, " B =", pub_B
    print "(I6,A,I6,A,I6)", g, " ^ ? mod ", prime, " = ", pub_A
    print "(I6,A,I6,A,I6)", g, " ^ ? mod ", prime, " = ", pub_B

    print *
    print "(A)", "Enter a message to send securely:"

    read (*,'(A)') temp_input
    message = trim(temp_input)
    call encrypt_message(message, shared_A, ciphertext)

    print *
    write(*,'(A)', advance='no') "Alice encrypts the message: "
    do count_i = 1, size(ciphertext)
        write(*,'(I0,1X)', advance='no') ciphertext(count_i)
    end do
    print *
    print *

    call decrypt_message(ciphertext, shared_B, returned_message)
    print "(A,A)", "Bob receives and decrypts: ", returned_message
    print *

end program main
