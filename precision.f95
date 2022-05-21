module precision

implicit none
integer, parameter :: sp = selected_real_kind(5,30)
integer, parameter :: dp = selected_real_kind(8,100)
integer, parameter :: qp = selected_real_kind(18,400)
integer, parameter :: ip1 = selected_int_kind(2)
integer, parameter :: ip2 = selected_int_kind(4)
integer, parameter :: ip3 = selected_int_kind(8)
integer, parameter :: ip4 = selected_int_kind(10)
integer, parameter :: ip5 = selected_int_kind(20)

end module precision
