# hask-elapsed

Computes the number of seconds elapsed between two moments in time.

The input to be supplied on the standard input stream, as two lines representing the start and end points of the 
required time interval in the ISO 8601 format “YYYY-MM-DDTHH:MM:SS”, with four digit years and without fractions 
of a second. For example:

      2018-06-25T21:53:35
      2018-06-25T22:53:36

Output will be the amount of seconds between the 2 dates. e.g.

    3601
    
This is a learning exercise to get my head around Haskell. So not sure if this is the most "Haskelly" way of doing things.

## Usage
Build using:

    stack build 
    
Then run using:

    stack exec hask-elapsed-exe
    
Test using:
    
    stack test
