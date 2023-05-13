package ServerTime;

use v5.36;

use Exporter qw(import);
use Time::HiRes;

our @EXPORT_OK = qw(server_time);

sub server_time :prototype() { goto \&Time::HiRes::time }

1;

