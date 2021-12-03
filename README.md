# FindingBackbone
Rapid algorithm for finding the current-carrying backbones in the two-dimensional percolation model

designed and implemented in gFortran by Wei-Guo Yin in 1999 @ Fudan University, Shanghai, China

References:
*   [1] Wei-Guo Yin, and R. Tao, “Rapid Algorithm for identifying backbones in the two-dimensional percolation model,” 
        Int. J. Mod. Phys. C: Physics and Computers 14, 1427 (2003).
*   [2] Wei-Guo Yin, and R. Tao, “Algorithm for finding two-dimensional site-percolation backbones,” Physica B 279, 84 (2000).

Usage:
* TO change the linear size, edit PARAMETER(n=10) in globvar.inc
* make new
* To change the number of samples, edit the VOLUME OF SAMPLES entry in input.dat
* Run the binary `fb'
