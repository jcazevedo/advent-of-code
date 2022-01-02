set fp [open "inputs/11.input" r]
set file_data [read $fp]
close $fp

set grid [list {*}$file_data]
set step_n 0
set total_flashes 0
set first_total_flash -1

proc step {grid} {
    global total_flashes
    global first_total_flash
    global step_n

    set step_n [incr step_n]

    set H [llength $grid]
    set W [string length [lindex $grid 0]]
    set to_flash {}
    set current_flashes 0

    proc inc {i j inc_zeros} {
        upvar H H
        upvar W W
        upvar grid grid
        upvar to_flash to_flash
        upvar current_flashes current_flashes

        if {$i >= 0 && $i < $H && $j >= 0 && $j < $W} {
            set v [string index [lindex $grid $i] $j]
            if {$inc_zeros || $v != 0} {
                set nv [expr {$v + 1}]
                if {$nv > 9} {
                    set nv 0
                    set idx {}
                    lappend idx $i
                    lappend idx $j
                    lappend to_flash $idx
                    set current_flashes [incr current_flashes]
                }
                lset grid $i [string replace [lindex $grid $i] $j $j $nv]
            }
        }
    }

    for {set i 0} {$i < $H} {incr i} {
        for {set j 0} {$j < $W} {incr j} {
            inc $i $j 1
        }
    }

    set dirs {{-1 0} {-1 1} {0 1} {1 1} {1 0} {1 -1} {0 -1} {-1 -1}}
    while {[llength $to_flash] > 0} {
        set curr [lindex $to_flash 0]
        set to_flash [lreplace $to_flash 0 0]
        for {set d 0} {$d < [llength $dirs]} {incr d} {
            set next_dir [lindex $dirs $d]
            set ni [expr {[lindex $curr 0] + [lindex $next_dir 0]}]
            set nj [expr {[lindex $curr 1] + [lindex $next_dir 1]}]
            inc $ni $nj 0
        }
    }

    set total_flashes [expr {$total_flashes + $current_flashes}]
    if {$first_total_flash == -1 && $current_flashes == [expr {$H * $W}]} {
        set first_total_flash $step_n
    }

    return $grid
}

set steps 100

for {set i 0} {$i < $steps} {incr i} {
    set grid [step $grid]
}

while {$first_total_flash == -1} {
    set grid [step $grid]
}

puts [concat "Part 1: " $total_flashes]
puts [concat "Part 2: " $first_total_flash]

exit 0
