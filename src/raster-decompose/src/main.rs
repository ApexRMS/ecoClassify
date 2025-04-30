use clap::Parser;
use rayon::prelude::*;
mod raster_functions;
use raster_functions::*;
use crate::raster_functions::read_bands;

#[derive(Parser)]
#[clap(name = "geotiff-decomposer")]
struct Args {
    #[clap(short, long)]
    input: String,

    #[clap(short, long)]
    output: String,

    #[clap(long, default_value = "3")]
    pca_components: usize,

    #[clap(long, default_value = "3")]
    texture_kernel: usize,

    #[clap(long, default_value = "4")]
    jobs: usize,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    rayon::ThreadPoolBuilder::new().num_threads(args.jobs).build_global()?;

    // Load and normalize bands
    let band_data = read_bands(&args.input)?; // <-- returns BandData now
    let normalized: Vec<_> = band_data.bands.iter()
        .map(|b| normalize_band(b))
        .collect();

    // Stack to tabular
    let base_table = stack_bands(&normalized);

    // PCA
    let pca_table = compute_pca(&base_table, args.pca_components)?;

    // Texture feature (mean)
    let texture_means: Vec<_> = normalized
        .par_iter()
        .map(|b| local_mean(b, args.texture_kernel))
        .collect();
    let texture_table = stack_bands(&texture_means);

    // Concatenate all tables
    let final_table = ndarray::concatenate![ndarray::Axis(1), base_table, pca_table, texture_table];

    // Create final headers
    let mut headers = Vec::new();

    // 1. Original normalized band names
    headers.extend(band_data.names.iter().map(|n| format!("{}_norm", n)));

    // 2. PCA component names
    for i in 1..=args.pca_components {
        headers.push(format!("PCA_{}", i));
    }

    // 3. Texture feature names (mean)
    headers.extend(band_data.names.iter().map(|n| format!("{}_localmean", n)));

    // Save with headers
    write_csv(&final_table, &headers, &args.output)?;
    Ok(())
}
