BEGIN {
    FS = "|";
    #lowest_score[""] = 0;
}



{
    if (lowest_student[$2] > $1 || lowest_student[$2] == "") {
	lowest_student[$2] = $1;
	student_score[$2] = $3;
    }
}    


END {
    for (course in lowest_student) {
	print course, student_score[course];
    }
}




