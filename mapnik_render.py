from __future__ import print_function

import argparse
import os
import shutil
import subprocess
import sys
import tempfile

import mapnik


def render(width, height, min_x, min_y, max_x, max_y, srs):
    print("Setting up map...", file=sys.stderr)
    map = mapnik.Map(width, height, srs=srs)
    map.background = mapnik.Color('#ffffff')

    style = mapnik.Style()
    rule = mapnik.Rule()

    line_symbolizer = mapnik.LineSymbolizer(mapnik.Color('#000000'), 0.6)
    line_symbolizer.smooth = 0.0
    rule.symbols.append(line_symbolizer)
    style.rules.append(rule)
    map.append_style('border_lines', style)

    ds = mapnik.Shapefile(file='data/TM_WORLD_BORDERS-0.3.shp')
    layer = mapnik.Layer('borders')
    layer.srs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

    layer.datasource = ds
    layer.styles.append('border_lines')

    map.layers.append(layer)

    bounds = mapnik.Box2d(min_x, min_y, max_x, max_y)
    map.zoom_to_box(bounds)

    result = mapnik.Image(width, height)
    mapnik.render(map, result)
    return result


def main(args):
    image = render(args.width, args.height, args.min_x, args.min_y,
                   args.max_x, args.max_y, args.srs)
    sys.stdout.write(image.tostring('png'))


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("width", type=int)
    parser.add_argument("height", type=int)
    parser.add_argument("min_x", type=float)
    parser.add_argument("min_y", type=float)
    parser.add_argument("max_x", type=float)
    parser.add_argument("max_y", type=float)
    parser.add_argument("srs", type=str)
    args = parser.parse_args()
    print(repr(args), file=sys.stderr)
    main(args)
