import numpy as np
import scipy
from skimage.filters import window


def get_1d_psd(
    data,
    bin_size=1,
    return_power_spectrogram=False,
    transform_type="DCT",
    average_power_band=True,
    fft_window=True,
):
    """
    bin_size: number of frequency coefficients in each bin. Incomplete final
    bins are omitted.
    The start and end frequency are relative to the window size.
    a frequency of 1 is the coeffiecient with 2 * pi()
    Transform_type: DCT or DFT
    """

    assert (
        data.shape[0] == data.shape[1]
    ), "This method is only implemented for square data arrays"
    N = data.shape[0]

    if transform_type == "DFT":
        xx, yy = np.mgrid[:N, :N]
        # a 2d array with the frequency this component corresponds to
        # in one dimension for an dft this is equivalent to the index position
        # so k=1 is equivalent to 2*pi(). all frequencies larger than n/2 are
        # "aliased" and thus equivalent to the n - nfreq
        center_freq = np.sqrt(xx ** 2 + yy ** 2)
        center_freq = scipy.fft.fftshift(center_freq)

        data = data.copy()
        if fft_window == True:
            hann_window = window("hann", data.shape)
            data *= hann_window
            hann_sum = np.sum(hann_window)

        power_spectrum = np.abs(scipy.fft.fft2(data, norm="ortho")) ** 2
        # power_spectrum = np.abs(scipy.fft.fft2(data, norm=None)) ** 2
        # power_spectrum = np.abs(scipy.fft.fft2(data, norm="forward"))
        power_spectrum = scipy.fft.fftshift(power_spectrum)
        power_spectrum /= N**2
        if fft_window == True:
            power_spectrum *= N**2 / np.sum(hann_window**2)

        # amplitude_spectrum = amplitude_spectrum**2 / N**2
        
        # FFT jumps by 2*pi increments
        bins = np.arange(int((N / 2) // bin_size) + 1) * bin_size
    elif transform_type == "DCT":
        xx, yy = np.mgrid[:N, :N]
        # A 2D array with the frequency this component corresponds to
        # in one dimension for an DCT the frequency jups by a half period
        # k*pi.
        center_freq = np.sqrt(xx ** 2 + yy ** 2) / 2 

        amplitude_spectrum = scipy.fftpack.dctn(data, type=2, norm="ortho")
        power_spectrum = amplitude_spectrum**2 / N**2

        bins = np.arange(N // bin_size + 1) * bin_size / 2

    else:
        raise Exception(f"Unknown transform_type {transform_type}")


    # Exclude the incomplete final bin, including coefficients on its lower edge.
    complete_bins = center_freq < bins[-1]
    psd, freqs = np.histogram(
        center_freq[complete_bins], bins, weights=power_spectrum[complete_bins]
    )

    if average_power_band:
        bin_sample_count, _ = np.histogram(center_freq[complete_bins], bins)
        psd = psd / bin_sample_count

    if not return_power_spectrogram:
        return psd, freqs
    else:
        return psd, freqs, power_spectrum
