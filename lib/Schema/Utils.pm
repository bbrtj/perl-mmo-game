package Schema::Utils;

use Exporter qw(import);
use Exception::RecordDoesNotExist;
use Exception::SearchCriteriaTooVague;

use header -noclean;

our @EXPORT_OK = qw(
	ensure_single
);

sub ensure_single ($rs)
{
	my $found = $rs->next;
	Exception::RecordDoesNotExist->throw unless $found;
	Exception::SearchCriteriaTooVague->throw if $rs->next;

	return $found;
}
