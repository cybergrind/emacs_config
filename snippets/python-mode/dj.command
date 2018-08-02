# -*- mode: snippet -*-
# name: dj.command
# key: command
# --
import logging

from django.core.management.base import BaseCommand

from retail.models import RetailInventory, FTSInventory


logger = logging.getLogger('${1:fix_multistore}')


class Command(BaseCommand):
    help = '${2:In multistore: set in_stock=0 for items without parent}'

    def add_arguments(self, parser):
        parser.add_argument('--group-id', action='store', type=int, required=True)

    def handle(self, *args, group_id=None, **options):
        with FTSInventory.turn_off_sync():
            $0pass
