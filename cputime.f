	subroutine cputime(Tijd)
        real Tijd, Tijdarray(2)
	Tijd = Etime(Tijdarray)
	end

C Etime is an intrinsic function of gFortran,
C
C      ETime(TArray)
C
C ETime: REAL(KIND=1) function.
C
C TArray: REAL(KIND=1); DIMENSION(2); INTENT(OUT).
C
C Intrinsic groups: unix.
C
C Description:
C
C Returns the number of seconds of runtime since the start of the process's
C execution as the function value, and the user and system components of
C this in `TArray(1)' and `TArray(2)' respectively. The functions' value
C is equal to `TArray(1) + TArray(2)'.
