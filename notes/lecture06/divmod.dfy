function IntegerDivision(num: int, denom: int) : (int, int)
  requires denom > 0
  requires num >= 0
  ensures num == denom * IntegerDivision(num, denom).0 + IntegerDivision(num, denom).1
  ensures 0 <= IntegerDivision(num, denom).1 < denom
  decreases num
{
  if num < denom then
    (0, num)
  else
    var (q', r') := IntegerDivision(num - denom, denom);
    (q' + 1, r')
}