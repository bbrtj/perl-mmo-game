use Test2::Thunderhorse;
use HTTP::Request::Common;
use Web;

use testheader;

my $app = Web->new;

http $app, GET '/';
http_status_is 200;
like http->text, qr/Angielski|English/, 'content ok';

done_testing();

